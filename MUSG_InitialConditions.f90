module MUSG_InitialConditions
    !### Initial condition assignment routines for MODFLOW-USG
    ! Handles assignment of initial heads/depths from various sources:
    ! - CSV files
    ! - Tecplot files
    ! - Surface elevation
    ! - Functions of Z
    ! - Depth-saturation tables
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, Msg, ErrMsg, TmpSTR, FMT_R8, UnitsOfLength, status, OpenAscii, FreeUnit, LwrCse
    use ErrorHandling, only: ERR_INVALID_INPUT, HandleError
    use MUSG_Core, only: ModflowDomain
    use Materials, only: UnsaturatedFunctionType
    
    implicit none
    private
    
    public :: InitialHeadFromCSVFile, GWFInitialHeadFromTecplotFile
    public :: GWFInitialHeadEqualsSurfaceElevation, InitialHeadFunctionOfZtoGWF
    public :: InitialHeadFromDepthSatToGWF, SWFInitialHeadFromTecplotFile
    public :: myMOD  ! Also used in Modflow_USG.f90
    
    contains
    
    !----------------------------------------------------------------------
    subroutine InitialHeadFromCSVFile(FnumMUT,domain)
        implicit none

        type(ModflowDomain), intent(inout) :: domain
        
        integer :: i
        
        integer(i4), intent(in) :: FnumMUT
        integer(i4) :: FNumRestart
        character(MAX_STR) :: FNameRestart
        
        character(4000) :: line

        read(FnumMUT,'(a)') FNameRestart
        call OpenAscii(FNumRestart,FNameRestart)
        call Msg(TRIM(domain.name)//' starting heads from CSV file: '//trim(FNameRestart))

        read(FNumRestart,'(a)',iostat=status) line
        call Msg(TRIM(domain.name)//' CSV file header: '//trim(line))
        
        read(FNumRestart,*) (domain%cell(i)%StartingHeads,i=1,domain%nCells)
        call Msg('First 10 starting heads:')
        do i=1,10
            write(TmpSTR,'(a,i5,a,'//FMT_R8//')')' Starting head cell ',i,': ',domain%cell(i)%StartingHeads
            call Msg(trim(TmpSTR))
        end do
        
        call FreeUnit(FNumRestart)
                
    end subroutine InitialHeadFromCSVFile
    
    !----------------------------------------------------------------------
    subroutine GWFInitialHeadFromTecplotFile(FnumMUT,domain)
        implicit none

        type(ModflowDomain), intent(inout) :: domain
        
        integer :: i
        
        integer(i4), intent(in) :: FnumMUT
        integer(i4) :: FNumRestart
        character(MAX_STR) :: FNameRestart
        
        character(4000) :: line

        read(FnumMUT,'(a)') FNameRestart
        call OpenAscii(FNumRestart,FNameRestart)
        call Msg( 'GWF restart from tecplot file: '//trim(FNameRestart))

        FindStartString: do
            read(FNumRestart,'(a)',iostat=status) line
            if(status /= 0) then
                call FreeUnit(FNumRestart)
                return
            end if
            
            if(index(line,'DT=(SINGLE )').gt.0) then
                read(FNumRestart,*) (domain%cell(i)%StartingHeads,i=1,domain%nCells)
                call Msg('First 10 starting heads:')
                do i=1,10
                    write(TmpSTR,'(a,i5,a,'//FMT_R8//')')' Starting head cell ',i,': ',domain%cell(i)%StartingHeads
                    call Msg(trim(TmpSTR))
                end do
                exit FindStartString
            end if
        end do FindStartString
        
        call FreeUnit(FNumRestart)
                
    end subroutine GWFInitialHeadFromTecplotFile
    
    !----------------------------------------------------------------------
    subroutine GWFInitialHeadEqualsSurfaceElevation(domain)
        implicit none

        type(ModflowDomain), intent(inout) :: domain
        
        integer :: i, modi, nsurf
        
        do i=1,domain%nCells
            modi=mod(i,domain%nodelay)
            if (modi==0) modi=domain%nodelay
            nsurf = modi 

            domain%cell(i)%StartingHeads = domain%cell(nsurf)%Top
        end do

                
    end subroutine GWFInitialHeadEqualsSurfaceElevation
    
    !----------------------------------------------------------------------
    subroutine InitialHeadFunctionOfZtoGWF(FNumMUT,domain)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: domain

        integer(i4) :: i, j
        integer(i4) :: npairs
        real(dp) :: t
                
        character(256) :: instruction

        real(dp) :: zp(1000)
        real(dp) :: InitHead(1000)
        zp(:) = 0
        InitHead(:) = 0


        call Msg('                Z              Head')

        npairs=0
        read_zhead_pairs:do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg('end gwf initial head function of z')
                exit read_zhead_pairs
            else
                npairs=npairs+1
                read(instruction,*,iostat=status) zp(npairs),InitHead(npairs)

                if(npairs > 1) then
                    if(zp(npairs) <= zp(npairs-1)) then
                        call HandleError(ERR_INVALID_INPUT, 'Z values must be entered in ascending order', 'InitialHeadFunctionOfZtoGWF')
                    endif
                endif

                if(status /= 0) then
                    call HandleError(ERR_INVALID_INPUT, 'Bad z-head pair', 'InitialHeadFunctionOfZtoGWF')
                endif
                
                write(TmpSTR,'(i8,2x,2('//FMT_R8//'))') npairs,zp(npairs),InitHead(npairs)
                call Msg(trim(TmpSTR))

            endif
        end do read_zhead_pairs

        do i=1,domain%nCells
            do j=1,npairs-1
                if(domain%cell(i)%z >= zp(j) .and. domain%cell(i)%z <= zp(j+1)) then  ! interpolate
                    t=(domain%cell(i)%z-zp(j))/(zp(j+1)-zp(j))
                    domain%cell(i)%StartingHeads=(1.0-t)*InitHead(j)+t*InitHead(j+1)
                end if
            end do
        end do
        
        call Msg('Assumed units of length are '//TRIM(UnitsOfLength))


    end subroutine InitialHeadFunctionOfZtoGWF
    
    !----------------------------------------------------------------------
    subroutine InitialHeadFromDepthSatToGWF(FNumMUT,domain)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: domain

        integer(i4) :: i
        integer(i4) :: npairs
        
        integer(i4) :: iCell
        real(dp) :: depth
        real(dp) :: sw
                
        character(256) :: instruction

        real(dp) :: dpth(1000)
        real(dp) :: satn(1000)
        dpth(:) = 0
        satn(:) = 0


        call Msg('                Depth               Sat')

        npairs=0
        read_DepthSaturation_pairs:do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg('end gwf initial head function of z')
                exit read_DepthSaturation_pairs
            else
                npairs=npairs+1
                read(instruction,*,iostat=status) dpth(npairs),satn(npairs)

                if(npairs > 1) then
                    if(dpth(npairs) <= dpth(npairs-1)) then
                        call HandleError(ERR_INVALID_INPUT, 'Depth values must be entered in ascending order', 'InitialHeadFromDepthSatToGWF')
                    endif
                endif

                if(status /= 0) then
                    call HandleError(ERR_INVALID_INPUT, 'Bad z-saturation pair', 'InitialHeadFromDepthSatToGWF')
                endif
                
                write(TmpSTR,'(i8,2x,2('//FMT_R8//'))') npairs,dpth(npairs),satn(npairs)
                call Msg(trim(TmpSTR))

            endif
        end do read_DepthSaturation_pairs
        
        
        do i=1,domain%nCells
            
            !find the cell number on the surface that corresponds to cell i
            iCell = myMOD(i,domain%nodelay)
            depth=domain%cell(iCell)%Top-domain%cell(i)%z

            call interpol_table(npairs,dpth,satn,depth,sw)
            
            domain%cell(i)%StartingHeads=pw_from_sw(i,sw,domain)

        end do
        
        call Msg('Assumed units of length are '//TRIM(UnitsOfLength))


    end subroutine InitialHeadFromDepthSatToGWF
    
    !----------------------------------------------------------------------
    subroutine SWFInitialHeadFromTecplotFile(FnumMUT,domain)
        implicit none

        type(ModflowDomain), intent(inout) :: domain
        
        integer :: i
        
        integer(i4), intent(in) :: FnumMUT
        integer(i4) :: FNumRestart
        character(MAX_STR) :: FNameRestart
        
        character(4000) :: line

        read(FnumMUT,'(a)') FNameRestart
        call OpenAscii(FNumRestart,FNameRestart)
        call Msg( 'SWF restart from tecplot file: '//trim(FNameRestart))

        FindStartString: do
            read(FNumRestart,'(a)',iostat=status) line
            if(status /= 0) then
                call FreeUnit(FNumRestart)
                return
            end if
            
            if(index(line,'DT=(SINGLE )').gt.0) then
                read(FNumRestart,*) (domain%cell(i)%StartingHeads,i=1,domain%nCells)
                call Msg('First 10 starting heads:')
                do i=1,10
                    write(TmpSTR,'(a,i5,a,'//FMT_R8//')')' Starting head cell ',i,': ',domain%cell(i)%StartingHeads
                    call Msg(trim(TmpSTR))
                end do
                exit FindStartString
            end if
        end do FindStartString
        
        call FreeUnit(FNumRestart)
                
    end subroutine SWFInitialHeadFromTecplotFile
    
    !----------------------------------------------------------------------
    ! Helper functions (these are used by InitialHeadFromDepthSatToGWF)
    ! Note: These are currently in Modflow_USG.f90 and may need to be moved
    ! or made accessible
    
    !----------------------------------------------------------------------
    function myMOD(i, n) result(mod_result)
        ! Custom modulo function that returns n when mod(i,n) == 0, otherwise returns mod(i,n)
        implicit none
        integer(i4), intent(in) :: i, n
        integer(i4) :: mod_result
        integer(i4) :: modi
        
        modi = mod(i, n)
        if (modi == 0) then
            mod_result = n
        else
            mod_result = modi
        end if
    end function myMOD
    
    !----------------------------------------------------------------------
    subroutine interpol_table(nsize,xtable,ytable,xval,yval)
        implicit none

        integer(i4), intent(in) :: nsize
        real(dp), intent(in) :: xval
        real(dp), intent(out) :: yval
        real(dp), intent(in) :: xtable(1000)
        real(dp), intent(in) :: ytable(1000)

        integer(i4) :: j, jlower, jupper, jmid

        jlower=0
        jupper=nsize+1
        10    if(jupper-jlower.gt.1)then
            jmid=(jupper+jlower)/2
            if((xtable(nsize).gt.xtable(1)).eqv.(xval.gt.xtable(jmid)))then
                jlower=jmid
            else
                jupper=jmid
            endif
            go to 10
        endif
        j=jlower


        if(j==0) then
            yval=ytable(1)
        else if (j==nsize) then
            yval=ytable(nsize)
        else
            yval = ytable(j) + (xval-xtable(j))*(ytable(j+1)-ytable(j)) / (xtable(j+1)-xtable(j))
        endif

        return
    end subroutine interpol_table
    
    !----------------------------------------------------------------------
    function pw_from_sw(iCell,sw,domain)
        !
        !  ...Compute the pressure for water saturation, sw
        !     Written by Kerry MacQuarrie, Nov. 1995
        !
        ! rgm 2025  modfied for Modflow unsat functions
    
        ! *** only implemented for van Genuchten or Brooks-Corey functions (method 4 in modflow)

        !
        implicit none
        type(ModflowDomain), intent(in) :: domain

        integer(i4), intent(in) :: iCell   ! cell number
        real(dp) :: pw_from_sw
        !
        real(dp), intent(in) :: sw
        real(dp) :: c1, eps_conv, aconstant, se
        real(dp) :: v_gamma, v_beta, v_alpha, gamma
        real(dp) :: sat_1, sat_2, sat_shift, pres_1, pres_2, dp_ds

        parameter (c1 = 1.0d0, eps_conv = 1.0d-4)
        parameter (sat_shift = 1.0d-6)
        parameter (aconstant = c1-eps_conv)


        ! Compute effective saturation
        se = (sw - domain%Cell(iCell)%Sr) / (c1 - domain%Cell(iCell)%Sr)
        ! Compute some constants
        gamma = 1.-1./domain%Cell(iCell)%Beta
        v_gamma = c1/gamma
        v_beta  = c1/domain%Cell(iCell)%Beta
        v_alpha = c1/domain%Cell(iCell)%Alpha

        if (se .gt. aconstant) then ! use linear interpolation

            sat_1  = aconstant
            pres_1 = v_alpha * ( (sat_1)**(-v_gamma) - c1 )**(v_beta)
            sat_2  = c1 ! note that the pressure at sat=1.0 is 0.0
            dp_ds  = pres_1/eps_conv
            pw_from_sw = dp_ds*(se-sat_1) + pres_1

        else if(se .lt. eps_conv) then ! use linear interpolation

            sat_1  = eps_conv
            pres_1 = v_alpha * ( (sat_1)**(-v_gamma) - c1 )**(v_beta)
            sat_2  = eps_conv + sat_shift
            pres_2 = v_alpha * ( (sat_2)**(-v_gamma) - c1 )**(v_beta)
            dp_ds  = (pres_2 - pres_1)/sat_shift
            pw_from_sw = dp_ds*(se-sat_1) + pres_1

        else ! invert van Genuchten relation directly
            pw_from_sw = v_alpha * ( (se)**(-v_gamma) - c1 )**(v_beta)

        end if

        ! Substract air entry pressure (which is actually "-") and change sign

        if(UnsaturatedFunctionType(domain%cell(iCell)%idZone) == 'Van Genuchten') then
            pw_from_sw = -pw_from_sw
        else if(UnsaturatedFunctionType(domain%cell(iCell)%idZone) == 'Brooks-Corey') then
            pw_from_sw = -(pw_from_sw - domain%cell(icell)%Brooks)
        endif

    end function pw_from_sw

end module MUSG_InitialConditions

