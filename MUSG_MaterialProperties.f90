module MUSG_MaterialProperties
    !### Material property assignment for MODFLOW-USG
    ! Handles assignment of material properties to domains (GWF, CLN, SWF)
    ! Includes both direct property assignment and material database assignment
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, Msg, ErrMsg, TmpSTR, FMT_R4, FMT_R8
    use GeneralRoutines, only: UnitsOfLength, UnitsOfTime, LengthConverter, TimeConverter
    use GeneralRoutines, only: bcheck, chosen, WarnMsg
    use GeneralRoutines, only: USERBIN, DefineUserbin
    use ErrorHandling, only: ERR_INVALID_INPUT, ERR_FILE_IO, HandleError
    use GeneralRoutines, only: OpenAscii, FreeUnit
    use MUSG_Core, only: ModflowDomain
    use materials
    
    implicit none
    private
    
    public :: AssignAlphatoDomain, AssignBetatoDomain, AssignBrookstoDomain
    public :: AssignKhtoDomain, AssignKvtoDomain, AssignSytoDomain, AssignSstoDomain, AssignSrtoDomain
    public :: AssignSgcltoDomain, AssignFSKINtoDomain, AssignStartingDepthtoDomain, AssignStartingHeadtoDomain
    public :: AssignMaterialtoGWF, AssignMaterialtoCLN, AssignMaterialtoSWF
    public :: CLN_AssignCircularRadius, CLN_AssignRectangularWidthHeight
    public :: AssignManningtoSWF, AssignDepressiontoSWF, AssignObstructiontoSWF
    public :: AssignDepthForSmoothingtoSWF
    public :: MaterialAssignmentSummary
    
    contains
    
    !----------------------------------------------------------------------
    subroutine AssignAlphatoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)//'^(-1)' 

        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Alpha of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Alpha=value
            end if
        end do
    
    end subroutine AssignAlphatoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignBetatoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(dp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R8//')') value
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Beta of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Beta=value
            end if
        end do
    
    end subroutine AssignBetatoDomain

    !----------------------------------------------------------------------
    subroutine AssignBrookstoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(dp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R8//')') value
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Brooks of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Brooks=value
            end if
        end do
    
    end subroutine AssignBrookstoDomain

    !----------------------------------------------------------------------
    subroutine AssignKhtoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Kh of '//trim(TmpSTR))
        
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Kh=value
            end if
        end do
    
    end subroutine AssignKhtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignKvtoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Kv of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Kv=value
            end if
        end do
    
    end subroutine AssignKvtoDomain

    !----------------------------------------------------------------------
    subroutine AssignSytoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//')') value
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Sy of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Sy=value
            end if
        end do
    
    end subroutine AssignSytoDomain

    !----------------------------------------------------------------------
    subroutine AssignSstoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)//'^(-1)'
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Ss of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Ss=value
            end if
        end do
    
    end subroutine AssignSstoDomain

    !----------------------------------------------------------------------
    subroutine AssignSrtoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//')') value
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a Sr of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Sr=value
            end if
        end do
    
    end subroutine AssignSrtoDomain

    !----------------------------------------------------------------------
    subroutine AssignSgcltoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg('Assigning all chosen '//trim(domain%name)//' cells an Sgcl of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%Sgcl=value
            end if
        end do
    
    end subroutine AssignSgcltoDomain

    !----------------------------------------------------------------------
    subroutine AssignFSKINtoDomain(FNumMUT,domain)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfTime)//'^(-1)'
        call Msg('Assigning all chosen '//trim(domain%name)//' cells an FSKIN of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%FSKIN=value
            end if
        end do
    
    end subroutine AssignFSKINtoDomain

    !----------------------------------------------------------------------
    subroutine AssignStartingDepthtoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(dp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R8//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg('Assigning all chosen '//trim(domain%name)//' cells a starting depth of '//trim(TmpSTR))


        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%StartingHeads=domain%cell(i)%z+value
            end if
        end do
    
    end subroutine AssignStartingDepthtoDomain

    !----------------------------------------------------------------------
    subroutine AssignStartingHeadtoDomain(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(dp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R8//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg('Assigning all chosen '//trim(domain%name)//' cells starting heads of '//trim(TmpSTR))

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                domain%cell(i)%StartingHeads=value
            end if
        end do
    
    end subroutine AssignStartingHeadtoDomain

    !----------------------------------------------------------------------
    subroutine AssignMaterialtoGWF(FNumMUT, Domain) 
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        integer(i4) :: iMaterial
        logical, allocatable :: IsChosen(:)
        
        real(sp) :: LengthConversionFactor
        real(sp) :: TimeConversionFactor
        
        read(FNumMUT,*) iMaterial
        
        do i=1,nGWFMaterials
            if(iMaterial == GWF_MaterialID(i)) then
                iMaterial=i
                exit
            endif
        end do
        
        write(TmpSTR,'(i5)') iMaterial
        call Msg('Assigning all chosen '//trim(domain%name)//' cells properties of material '//trim(TmpSTR)//', '//trim(GWF_MaterialName(iMaterial)))
        
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Kh_Kx:             ',Kh_Kx(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'   '//TRIM(GWF_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Kv_Kz:             ',Kv_Kz(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'   '//TRIM(GWF_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Specific Storage:  ',SpecificStorage(iMaterial)  ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Specific Yield:    ',SpecificYield(iMaterial)    ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Porosity:          ',Porosity(iMaterial)         ,'     DIMENSIONLESS (for average linear velocity)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Alpha:             ',Alpha(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Beta:              ',Beta(iMaterial)             ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Sr:                ',Sr(iMaterial)               ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        select case(UnsaturatedFunctionType(iMaterial))
        case ('Van Genuchten')
            write(TmpSTR,'(a)')    'Unsaturated Function Type:   '//trim(UnsaturatedFunctionType(iMaterial))
            call Msg(TmpSTR)
        case ('Brooks-Corey')
            write(TmpSTR,'(a)')        'Unsaturated Function Type: '//trim(UnsaturatedFunctionType(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//')')'Brooks Corey Exponent:     ',BrooksCoreyExponent(iMaterial)
            call Msg(TmpSTR)
        case default
            call HandleError(ERR_INVALID_INPUT, 'Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported', 'AssignAlphatoDomain')
        end select
        
        LengthConversionFactor=LengthConverter(UnitsOfLength,GWF_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,GWF_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')'Material length unit:    '//trim(GWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(GWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')'Material time unit:    '//trim(GWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(GWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

        if(domain%name == 'GWF') then
            call PrepareMaterialTracker(domain%idMaterial,domain%nCells)
            allocate(IsChosen(domain%nCells))
            do i=1,domain%nCells
                IsChosen(i)=bcheck(domain%cell(i)%is,chosen)
            end do
            call CheckMaterialOverwrite('GWF','cells',IsChosen,domain%idMaterial,iMaterial,GWF_MaterialName, &
                ChosenZoneList(domain),domain%nMaterialOverwrites)
        endif
       
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                if(domain%name == 'GWF') then
                    domain%cell(i)%Kh=Kh_Kx(iMaterial)*LengthConversionFactor/TimeConversionFactor       ! L/T
                    domain%cell(i)%Kv=Kv_Kz(iMaterial)*LengthConversionFactor/TimeConversionFactor       ! L/T
                    domain%cell(i)%Ss=Specificstorage(iMaterial)/LengthConversionFactor                  ! 1/L
                    domain%cell(i)%Sy=SpecificYield(iMaterial)                                           ! -
                    domain%cell(i)%Porosity=Porosity(iMaterial)                                            ! for seepage velocity
                    domain%cell(i)%Alpha=Alpha(iMaterial)/LengthConversionFactor                         ! 1/L
                    domain%cell(i)%Beta=Beta(iMaterial)                                                  ! -
                    domain%cell(i)%Sr=Sr(iMaterial)                                                      ! -
                    
                    select case(UnsaturatedFunctionType(iMaterial))
                    case ('Van Genuchten')
                        domain%cell(i)%Brooks= -BrooksCoreyExponent(iMaterial)
                    case ('Brooks-Corey')
                        domain%cell(i)%Brooks=BrooksCoreyExponent(iMaterial)
                    case default
                        call HandleError(ERR_INVALID_INPUT, 'Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported', 'AssignBrookstoDomain')
                    end select

                endif
            end if
        end do
        
        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
            call Msg('Properties of material '//trim(TmpSTR)//', '//trim(GWF_MaterialName(iMaterial))//' after unit conversion')
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Kh_Kx:             ',Kh_Kx(iMaterial)*LengthConversionFactor/TimeConversionFactor  ,'     '//TRIM(UnitsOfLength)//'   '//TRIM(UnitsOfTime)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Kv_Kz:             ',Kv_Kz(iMaterial)*LengthConversionFactor/TimeConversionFactor  ,'     '//TRIM(UnitsOfLength)//'   '//TRIM(UnitsOfTime)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Specific Storage:  ',SpecificStorage(iMaterial)/LengthConversionFactor             ,'     '//TRIM(UnitsOfLength)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Specific Yield:    ',SpecificYield(iMaterial)                                      ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Porosity:          ',Porosity(iMaterial)                                           ,'     DIMENSIONLESS (for average linear velocity)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Alpha:             ',Alpha(iMaterial)/LengthConversionFactor                       ,'     '//TRIM(UnitsOfLength)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Beta:              ',Beta(iMaterial)                                               ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Sr:                ',Sr(iMaterial)                                                 ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            select case(UnsaturatedFunctionType(iMaterial))
            case ('Van Genuchten')
                write(TmpSTR,'(a)')    'Unsaturated Function Type:   '//trim(UnsaturatedFunctionType(iMaterial))
                call Msg(TmpSTR)
            case ('Brooks-Corey')
                write(TmpSTR,'(a)')        'Unsaturated Function Type: '//trim(UnsaturatedFunctionType(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,'//FMT_R4//')')'Brooks Corey Exponent:     ',BrooksCoreyExponent(iMaterial)
                call Msg(TmpSTR)
            case default
                call HandleError(ERR_INVALID_INPUT, 'Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported', 'AssignMaterialtoDomain')
            end select

        end if     
        
      
        continue
    
    end subroutine AssignMaterialtoGWF
    
    !----------------------------------------------------------------------
    subroutine AssignMaterialtoCLN(FnumMUT,CLN)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: CLN
        
        integer(i4) :: i
        integer(i4) :: iMaterial

        real(sp) :: LengthConversionFactor
        real(sp) :: TimeConversionFactor
        
        integer(i4) :: FNumTab
        character(MAX_STR) :: TabFile, TabFile2
        character(512) :: line
        integer(i4) :: nrows, ios
        real(sp) :: fdepth, farea, fwetperi, ftopwid
        logical :: exists
        logical, allocatable :: IsChosen(:)

        read(FNumMUT,*) iMaterial
        write(TmpSTR,'(i5)') iMaterial
        
        call Msg('Assigning all chosen '//trim(CLN%name)//' zones properties of material '//trim(TmpSTR)//', '//trim(CLN_Name(iMaterial)))
        select case(Geometry(iMaterial))
        case ('Circular')
            write(TmpSTR,'(a)')        'Geometry:           '//trim(Geometry(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Circular Radius:    ',CircularRadius(iMaterial)     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        case ('Rectangular')
            write(TmpSTR,'(a)')        'Geometry:           '//trim(Geometry(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Rectangular Width:  ',RectangularWidth(iMaterial)     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Rectangular Height: ',RectangularHeight(iMaterial)    ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        case ('General')
            write(TmpSTR,'(a)')        'Geometry:           '//trim(Geometry(iMaterial))
            call Msg(TmpSTR)
            if(len_trim(GeneralSectionTableFile(iMaterial))==0) then
                call HandleError(ERR_INVALID_INPUT, 'General CLN material requires a GeneralSectionTableFile column in CLN.csv', 'AssignMaterialtoCLN')
            end if
            call Msg('General section table: '//trim(GeneralSectionTableFile(iMaterial)))
        case default
            call HandleError(ERR_INVALID_INPUT, 'Geometry type '//trim(Geometry(iMaterial))//' not supported', 'CLN_AssignCircularRadius')
        end select

        write(TmpSTR,'(a)')            'Direction:          '//Direction(iMaterial)
        call Msg(TmpSTR)
        
        write(TmpSTR,'(a)')            'Flow Treatment:     '//FlowTreatment(iMaterial) 
        call Msg(TmpSTR)

        write(TmpSTR,'(a,'//FMT_R4//',a)')    'Longitudinal K:     ',LongitudinalK(iMaterial)        ,'     '//TRIM(CLN_LengthUnit(iMaterial))//'   '//TRIM(CLN_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')    'Infill Porosity:    ',InfillPorosity(iMaterial)       ,'     DIMENSIONLESS (for average linear velocity)'
        call Msg(TmpSTR)
        if(InfillPorosity(iMaterial) < 1.0) then
            call Msg('Note: CLN average linear velocity = Darcy / Infill Porosity')
        end if
        
        LengthConversionFactor=LengthConverter(UnitsOfLength,CLN_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,CLN_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')'Material length unit:    '//trim(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')'Material time unit:    '//trim(CLN_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(CLN_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

        call PrepareMaterialTracker(CLN%idCLNMaterial,CLN%nZones)
        allocate(IsChosen(CLN%nZones))
        do i=1,CLN%nZones
            IsChosen(i)=bcheck(CLN%zone(i)%is,chosen)
        end do
        call CheckMaterialOverwrite(trim(CLN%name),'zones',IsChosen,CLN%idCLNMaterial,iMaterial,CLN_Name, &
            ChosenZoneList(CLN),CLN%nMaterialOverwrites)
      
        do i=1,CLN%nZones
            if(bcheck(CLN%zone(i)%is,chosen)) then
                select case(Geometry(iMaterial))
                case ('Circular')
                    CLN%Geometry(i)=1
                    CLN%NCONDUITYP=CLN%NCONDUITYP+1
                case ('Rectangular')
                    CLN%Geometry(i)=2
                    CLN%NRECTYP=CLN%NRECTYP+1
                case ('General')
                    CLN%Geometry(i)=3
                    CLN%NGENSHPTYP=CLN%NGENSHPTYP+1
                case default
                    call HandleError(ERR_INVALID_INPUT, 'Geometry type '//trim(Geometry(iMaterial))//' not supported', 'CLN_AssignCircularRadius')
                end select

                select case(Direction(iMaterial))
                case ('Vertical')
                    CLN%Direction(i)=0
                case ('Horizontal')
                    CLN%Direction(i)=1
                case ('Angled')
                    CLN%Direction(i)=2
                case default
                    call HandleError(ERR_INVALID_INPUT, 'Direction type '//trim(Direction(iMaterial))//' not supported', 'AssignMaterialtoCLN')
                end select
                
                select case(FlowTreatment(iMaterial))
                case ('Confined\Laminar')
                    CLN%FlowTreatment(i)=1
                case ('Confined\Darcy-Weisbach')
                    CLN%FlowTreatment(i)=2
                case ('Confined\Heizen-Williams')
                    CLN%FlowTreatment(i)=3
                case ('Confined/Mannings')
                    CLN%FlowTreatment(i)=4
                case ('Unconfined\Laminar')
                    CLN%FlowTreatment(i)=-1
                case ('Unconfined\Darcy-Weisbach')
                    CLN%FlowTreatment(i)=-2
                case ('Unconfined\Heizen-Williams')
                    CLN%FlowTreatment(i)=-3
                case ('Unconfined/Mannings')
                    CLN%FlowTreatment(i)=-4
                case default
                    call HandleError(ERR_INVALID_INPUT, 'Flow treatment type '//trim(FlowTreatment(iMaterial))//' not supported', 'AssignMaterialtoCLN')
                end select
                
                CLN%CircularRadius(i)=CircularRadius(iMaterial)*LengthConversionFactor         ! L
                CLN%RectangularWidth(i)=RectangularWidth(iMaterial)*LengthConversionFactor         ! L
                CLN%RectangularHeight(i)=RectangularHeight(iMaterial)*LengthConversionFactor         ! L
                CLN%LongitudinalK(i)=LongitudinalK(iMaterial)*LengthConversionFactor/TimeConversionFactor         ! L/T
                CLN%InfillPorosity(i)=InfillPorosity(iMaterial)
                
                if(CLN%Geometry(i)==3) then
                    call DefineUserbin(USERBIN)
                    TabFile = trim(GeneralSectionTableFile(iMaterial))
                    inquire(file=TabFile,exist=exists)
                    if(.not. exists) then
                        TabFile2 = trim(USERBIN)//'\'//trim(TabFile)
                        inquire(file=TabFile2,exist=exists)
                        if(exists) TabFile = TabFile2
                    end if
                    if(.not. exists) then
                        call HandleError(ERR_FILE_IO, 'General section table file not found: '//trim(GeneralSectionTableFile(iMaterial)), 'AssignMaterialtoCLN')
                    end if
                    
                    call OpenAscii(FNumTab,TabFile)
                    
                    ! Count rows (skip blank/comment lines)
                    nrows = 0
                    do
                        read(FNumTab,'(a)',iostat=ios) line
                        if(ios/=0) exit
                        if(len_trim(line)==0) cycle
                        if(line(1:1)=='#') cycle
                        nrows = nrows + 1
                    end do
                    
                    if(nrows<=0) then
                        call FreeUnit(FNumTab)
                        call HandleError(ERR_INVALID_INPUT, 'General section table has no data rows: '//trim(TabFile), 'AssignMaterialtoCLN')
                    end if
                    
                    if(CLN%NGENTABROWS==0) then
                        CLN%NGENTABROWS = nrows
                        allocate(CLN%GenDepth(CLN%NGENTABROWS,CLN%nZones), &
                                 CLN%GenArea(CLN%NGENTABROWS,CLN%nZones), &
                                 CLN%GenWetPeri(CLN%NGENTABROWS,CLN%nZones), &
                                 CLN%GenTopWidth(CLN%NGENTABROWS,CLN%nZones), stat=ialloc)
                        call AllocChk(ialloc,'CLN general-section table arrays')
                        CLN%GenDepth = 0.0
                        CLN%GenArea = 0.0
                        CLN%GenWetPeri = 0.0
                        CLN%GenTopWidth = 0.0
                    else
                        if(nrows /= CLN%NGENTABROWS) then
                            call FreeUnit(FNumTab)
                            call HandleError(ERR_INVALID_INPUT, 'All general CLN section tables must have same number of rows (NGENTABROWS)', 'AssignMaterialtoCLN')
                        end if
                    end if
                    
                    rewind(FNumTab)
                    nrows = 0
                    do
                        read(FNumTab,'(a)',iostat=ios) line
                        if(ios/=0) exit
                        if(len_trim(line)==0) cycle
                        if(line(1:1)=='#') cycle
                        nrows = nrows + 1
                        read(line,*) fdepth, farea, fwetperi, ftopwid
                        CLN%GenDepth(nrows,i) = fdepth*LengthConversionFactor
                        CLN%GenArea(nrows,i) = farea*LengthConversionFactor*LengthConversionFactor
                        CLN%GenWetPeri(nrows,i) = fwetperi*LengthConversionFactor
                        CLN%GenTopWidth(nrows,i) = ftopwid*LengthConversionFactor
                    end do
                    
                    call FreeUnit(FNumTab)
                end if
            end if
        end do
        
        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
            call Msg('Properties of material '//trim(TmpSTR)//', '//trim(CLN_Name(iMaterial))//' after unit conversion')
            select case(Geometry(iMaterial))
            case ('Circular')
                write(TmpSTR,'(a)')        'Geometry:           '//trim(Geometry(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Circular Radius:    ',CircularRadius(iMaterial)*LengthConversionFactor      ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
            case ('Rectangular')
                write(TmpSTR,'(a)')        'Geometry:           '//trim(Geometry(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Rectangular Width:  ',RectangularWidth(iMaterial)*LengthConversionFactor      ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Rectangular Height: ',RectangularHeight(iMaterial)*LengthConversionFactor     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
            case default
                call HandleError(ERR_INVALID_INPUT, 'Geometry type '//trim(Geometry(iMaterial))//' not supported', 'AssignMaterialtoCLN')
            end select

            write(TmpSTR,'(a)')            'Direction:          '//Direction(iMaterial)
            call Msg(TmpSTR)
        
            write(TmpSTR,'(a)')            'Flow Treatment:     '//FlowTreatment(iMaterial) 
            call Msg(TmpSTR)

            write(TmpSTR,'(a,'//FMT_R4//',a)')    'Longitudinal K:     ',LongitudinalK(iMaterial)*LengthConversionFactor/TimeConversionFactor        ,'     '//TRIM(CLN_LengthUnit(iMaterial))//'   '//TRIM(CLN_TimeUnit(iMaterial))//'^(-1)'
            call Msg(TmpSTR)

        end if     
    
    end subroutine AssignMaterialtoCLN
    
    !----------------------------------------------------------------------
    subroutine AssignMaterialtoSWF(FnumMUT,domain)
        implicit none


        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        integer(i4) :: iMaterial
        logical, allocatable :: IsChosen(:)

        real(sp) :: LengthConversionFactor
        real(sp) :: TimeConversionFactor
        
        read(FNumMUT,*) iMaterial
        write(TmpSTR,'(i5)') iMaterial
        
        call Msg('Assigning all chosen '//trim(domain%name)//' zones properties of material '//trim(TmpSTR)//', '//trim(SWF_MaterialName(iMaterial)))
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Manning''s Coefficient:      ',ManningCoefficient(iMaterial)     ,'     '//TRIM(SWF_LengthUnit(iMaterial))//'^(-1/3)  '//TRIM(SWF_TimeUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Depression Storage Height:  ',DepressionStorageHeight(iMaterial) ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'Obstruction Storage Height: ',ObstructionStorageHeight(iMaterial),'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'SWF Smoothing Depth 1:      ',SWFSmoothingDepth1(iMaterial)      ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,'//FMT_R4//',a)')'SWF Smoothing Depth 2:      ',SWFSmoothingDepth2(iMaterial)      ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)

                
        LengthConversionFactor=LengthConverter(UnitsOfLength,SWF_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,SWF_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')'Material length unit:    '//trim(SWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(SWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')'Material time unit:    '//trim(SWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(SWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

        call PrepareMaterialTracker(domain%idSWFMaterial,domain%nZones)
        allocate(IsChosen(domain%nZones))
        do i=1,domain%nZones
            IsChosen(i)=bcheck(domain%zone(i)%is,chosen)
        end do
        call CheckMaterialOverwrite(trim(domain%name),'zones',IsChosen,domain%idSWFMaterial,iMaterial,SWF_MaterialName, &
            ChosenZoneList(domain),domain%nMaterialOverwrites)

        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                domain%Manning(i)=ManningCoefficient(iMaterial)*LengthConversionFactor**(-1/3)*TimeConversionFactor ! L^(-1/3) T
                domain%DepressionStorageHeight(i)=DepressionStorageHeight(iMaterial)*LengthConversionFactor         ! L
                domain%ObstructionStorageHeight(i)=ObstructionStorageHeight(iMaterial)*LengthConversionFactor       ! L
                domain%H1DepthForSmoothing(i)=SWFSmoothingDepth1(iMaterial)*LengthConversionFactor                  ! L
                domain%H2DepthForSmoothing(i)=SWFSmoothingDepth2(iMaterial)*LengthConversionFactor                  ! L
            end if                                                                               
        end do                                                                                   

        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
            call Msg('Properties of material '//trim(TmpSTR)//', '//trim(SWF_MaterialName(iMaterial))//' after unit conversion')
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Manning''s Coefficient:      ',ManningCoefficient(iMaterial)*LengthConversionFactor**(-1/3)*TimeConversionFactor      ,'     '//TRIM(UnitsOfLength)//'^(-1/3)  '//TRIM(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Depression Storage Height:  ',DepressionStorageHeight(iMaterial)*LengthConversionFactor ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Obstruction Storage Height: ',ObstructionStorageHeight(iMaterial)*LengthConversionFactor,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'SWF Smoothing Depth 1:      ',SWFSmoothingDepth1(iMaterial)*LengthConversionFactor      ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'SWF Smoothing Depth 2:      ',SWFSmoothingDepth2(iMaterial)      ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)

        end if     

    end subroutine AssignMaterialtoSWF

    !----------------------------------------------------------------------
    subroutine PrepareMaterialTracker(idAssigned,n)
        ! Allocate (or reset if the domain size changed) the per-cell/per-zone material tracker
        implicit none

        integer(i4), allocatable, intent(inout) :: idAssigned(:)
        integer(i4), intent(in) :: n

        if(allocated(idAssigned)) then
            if(size(idAssigned) == n) return
            deallocate(idAssigned)
        end if
        allocate(idAssigned(n),stat=ialloc)
        call AllocChk(ialloc,'Material assignment tracking array')
        idAssigned=0

    end subroutine PrepareMaterialTracker

    !----------------------------------------------------------------------
    function ChosenZoneList(Domain) result(ZoneList)
        implicit none

        type(ModflowDomain), intent(in) :: Domain
        character(len=:), allocatable :: ZoneList

        integer(i4) :: i
        character(16) :: str

        ZoneList=''
        do i=1,Domain%nZones
            if(btest(Domain%zone(i)%is,chosen)) then
                write(str,'(i0)') i
                ZoneList=ZoneList//' '//trim(str)
            end if
        end do
        if(len(ZoneList) == 0) ZoneList=' none'

    end function ChosenZoneList

    !----------------------------------------------------------------------
    subroutine CheckMaterialOverwrite(DomainName,ItemLabel,IsChosen,idAssigned,iMaterial,MatName,ZoneList,nOverwrites)
        ! Warn (non-fatal) when a material assignment replaces a different material assigned earlier
        ! in this build, then record the new assignment. The usual cause is a missing
        ! 'clear chosen zones': 'choose zone number' adds to the current zone selection.
        implicit none

        character(*), intent(in) :: DomainName
        character(*), intent(in) :: ItemLabel       ! 'cells' or 'zones'
        logical, intent(in) :: IsChosen(:)
        integer(i4), intent(inout) :: idAssigned(:)
        integer(i4), intent(in) :: iMaterial
        character(*), intent(in) :: MatName(:)
        character(*), intent(in) :: ZoneList
        integer(i4), intent(inout) :: nOverwrites

        integer(i4) :: k, nChosen, nReplaced, nPrev, nZonesChosen
        character(len=:), allocatable :: WarnStr

        nChosen=count(IsChosen)
        nReplaced=count(IsChosen .and. idAssigned > 0 .and. idAssigned /= iMaterial)
        nZonesChosen=0
        if(ZoneList /= ' none') nZonesChosen=count([(ZoneList(k:k) == ' ', k=1,len(ZoneList))])

        if(nReplaced > 0) then
            nOverwrites=nOverwrites+1
            write(TmpSTR,'(i0,a,i0,a,i0,a)') nReplaced,' of ',nChosen,' chosen '//DomainName//' '//ItemLabel// &
                ' already had a different material assigned and are now overwritten with material ',iMaterial, &
                ', '//trim(MatName(iMaterial))
            WarnStr=trim(TmpSTR)
            do k=1,size(MatName)
                if(k == iMaterial) cycle
                nPrev=count(IsChosen .and. idAssigned == k)
                if(nPrev == 0) cycle
                write(TmpSTR,'(a,i0,a,i0,a)') '    ',nPrev,' '//ItemLabel//' previously had material ',k,', '//trim(MatName(k))
                WarnStr=WarnStr//new_line('a')//trim(TmpSTR)
            end do
            WarnStr=WarnStr//new_line('a')//DomainName//' zones currently chosen:'//ZoneList
            if(nZonesChosen > 1) then
                WarnStr=WarnStr//new_line('a')//'Did you forget ''clear chosen zones'' before ''choose zone number''? '// &
                    '(choose zone number ADDS to the current zone selection). Ignore if the overwrite is intended.'
            else
                WarnStr=WarnStr//new_line('a')//'Ignore if a base material is intentionally being replaced in a subregion; '// &
                    'otherwise check for a missing ''clear chosen cells'' or ''clear chosen zones''.'
            end if
            call WarnMsg(WarnStr)
        end if

        where(IsChosen) idAssigned=iMaterial

    end subroutine CheckMaterialOverwrite

    !----------------------------------------------------------------------
    subroutine MaterialAssignmentSummary(Domain)
        ! Before the domain files are written: report how many cells (GWF) or zones (SWF, CLN) carry
        ! each material, and warn if a multi-zone domain ended up with one material after overwrites.
        implicit none

        type(ModflowDomain), intent(in) :: Domain

        select case(trim(Domain%name))
        case ('GWF')
            if(.not. allocated(Domain%idMaterial)) return
            call ReportMaterialCounts('GWF','cells',Domain%idMaterial,GWF_MaterialName,Domain%nZones,Domain%nMaterialOverwrites)
        case ('SWF')
            if(.not. allocated(Domain%idSWFMaterial)) return
            call ReportMaterialCounts('SWF','zones',Domain%idSWFMaterial,SWF_MaterialName,Domain%nZones,Domain%nMaterialOverwrites)
        case ('CLN')
            if(.not. allocated(Domain%idCLNMaterial)) return
            call ReportMaterialCounts('CLN','zones',Domain%idCLNMaterial,CLN_Name,Domain%nZones,Domain%nMaterialOverwrites)
        end select

    end subroutine MaterialAssignmentSummary

    !----------------------------------------------------------------------
    subroutine ReportMaterialCounts(DomainName,ItemLabel,idAssigned,MatName,nZones,nOverwrites)
        implicit none

        character(*), intent(in) :: DomainName
        character(*), intent(in) :: ItemLabel
        integer(i4), intent(in) :: idAssigned(:)
        character(*), intent(in) :: MatName(:)
        integer(i4), intent(in) :: nZones
        integer(i4), intent(in) :: nOverwrites

        integer(i4) :: k, nk, nUsed, kUsed, nNone
        character(len=:), allocatable :: WarnStr

        call Msg(' ')
        call Msg(DomainName//' material assignment summary:')
        nUsed=0
        kUsed=0
        do k=1,size(MatName)
            nk=count(idAssigned == k)
            if(nk == 0) cycle
            nUsed=nUsed+1
            kUsed=k
            write(TmpSTR,'(a,i5,a,i10,a)') '    material ',k,', '//trim(MatName(k))//': ',nk,' '//ItemLabel
            call Msg(trim(TmpSTR))
        end do
        nNone=count(idAssigned <= 0)
        if(nNone > 0) then
            write(TmpSTR,'(a,i10,a)') '    no material assigned: ',nNone,' '//ItemLabel
            call Msg(trim(TmpSTR))
        end if

        if(nZones > 1 .and. nUsed == 1 .and. nNone == 0 .and. nOverwrites > 0) then
            write(TmpSTR,'(a,i0,a,i0,a,i0,a)') 'All ',size(idAssigned),' '//DomainName//' '//ItemLabel//' ended up with material ', &
                kUsed,', '//trim(MatName(kUsed))//', although the '//DomainName//' domain has ',nZones, &
                ' zones and earlier material assignments were overwritten.'
            WarnStr=trim(TmpSTR)//new_line('a')//'Check that each zone has its intended material '// &
                '(did you forget ''clear chosen zones'' between zone selections?).'
            call WarnMsg(WarnStr)
        end if

    end subroutine ReportMaterialCounts

    !----------------------------------------------------------------------
    subroutine CLN_AssignCircularRadius(FnumMUT,CLN)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: CLN
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg(trim(CLN%name)//' CLN circular radius: '//trim(TmpSTR))


        do i=1,CLN%nZones
            if(bcheck(CLN%zone(i)%is,chosen)) then
                CLN%CircularRadius(i)=value
            end if
        end do
    
    end subroutine CLN_AssignCircularRadius
    
    !----------------------------------------------------------------------
    subroutine CLN_AssignRectangularWidthHeight(FnumMUT,CLN)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: CLN
        
        integer(i4) :: i
        real(sp) :: width, height
        
        read(FNumMUT,*) width, height
        write(TmpSTR,'(2'//FMT_R4//',a)')  width, height,'     '//TRIM(UnitsOfLength)
        call Msg(trim(CLN%name)//' CLN rectangular width and height: '//trim(TmpSTR))


        do i=1,CLN%nZones
            if(bcheck(CLN%zone(i)%is,chosen)) then
                CLN%RectangularWidth(i)=width
                CLN%RectangularHeight(i)=height
            end if
        end do
    
    end subroutine CLN_AssignRectangularWidthHeight

    !----------------------------------------------------------------------
    subroutine AssignManningtoSWF(FnumMUT,domain)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)//'^(-1/3)    '//TRIM(UnitsOfTime)
        call Msg(trim(domain%name)//' Manning''s coefficient of friction: '//trim(TmpSTR))


        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                domain%Manning(i)=value
            end if
        end do
    
    end subroutine AssignManningtoSWF

    !----------------------------------------------------------------------
    subroutine AssignDepressiontoSWF(FnumMUT,domain)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg(trim(domain%name)//' Depression Storage Height: '//trim(TmpSTR))


        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                domain%DepressionStorageHeight(i)=value
            end if
        end do
    
    end subroutine AssignDepressiontoSWF

    !----------------------------------------------------------------------
    subroutine AssignObstructiontoSWF(FnumMUT,domain)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(sp) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'('//FMT_R4//',a)') value,'     '//TRIM(UnitsOfLength)
        call Msg(trim(domain%name)//' Obstruction Storage Height: '//trim(TmpSTR))


        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                domain%ObstructionStorageHeight(i)=value
            end if
        end do
    
    end subroutine AssignObstructiontoSWF

    !----------------------------------------------------------------------
    subroutine AssignDepthForSmoothingtoSWF(FnumMUT,domain)
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain
        
        integer(i4) :: i
        real(dp) :: value1, value2
        
        read(FNumMUT,*) value1, value2
        write(TmpSTR,'('//FMT_R4//',a)') value1,'     '//TRIM(UnitsOfLength)
        call Msg(trim(domain%name)//' H1 depth for smoothing: '//trim(TmpSTR))
        write(TmpSTR,'('//FMT_R4//',a)') value2,'     '//TRIM(UnitsOfLength)
        call Msg(trim(domain%name)//' H2 depth for smoothing: '//trim(TmpSTR))

        
        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                domain%H1DepthForSmoothing(i)=value1
                domain%H2DepthForSmoothing(i)=value2
            end if
        end do

    end subroutine AssignDepthForSmoothingtoSWF

end module MUSG_MaterialProperties

