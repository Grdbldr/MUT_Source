module Raster
	use GeneralRoutines
	implicit none

    character(256) :: Raster_CMD  
    character(60) :: Raster_Read_CMD='read raster'
    character(60) :: Raster_Write_CMD='write raster'
    character(60) :: Raster_ltequals_CMD='less than equals'
    character(60) :: Raster_replaceValue_CMD='replace value'
    character(60) :: Raster_multiplier_CMD='multiplier'
    character(60) :: Raster_XYZto_CMD='XYZ to raster'
    character(60) :: Raster_End_CMD=	'end raster'

    
    character(80) :: rasterfile 
    
    integer, parameter :: MAXBINS=1000


    type RasterData
        real,  ALLOCATABLE :: rastval(:,:)   ! raster elevations
        real(dr),  ALLOCATABLE :: rastx(:)   ! raster x-coordinates
        real(dr),  ALLOCATABLE :: rasty(:)   ! raster y-coordinates
        real(dr) ::  vmiss = 0.0
        integer nxrast,nyrast
        real(dr) :: dxmin,dymin
        real(dr) :: dxmax,dymax
        logical :: have_raster
	    logical :: missing

        real,  ALLOCATABLE :: rastval2(:,:)   ! raster elevations
        real(dr),  ALLOCATABLE :: rastx2(:)   ! raster x-coordinates
        real(dr),  ALLOCATABLE :: rasty2(:)   ! raster y-coordinates
        real(dr) ::  vmiss2 = 0.0
        integer nxrast2,nyrast2
        real(dr) :: dxmin2,dymin2
        real(dr) :: dxmax2,dymax2
        logical :: have_raster2
	    logical :: missing2

        real(dr) ::  xspc  = 0.0
        real(dr) ::  yspc  = 0.0
        real(dr) ::  zspc  = 0.0
        
        
        integer :: nbinTot
        real(dr),  ALLOCATABLE :: vbin(:)
        integer(dr),  ALLOCATABLE :: nbin(:)
        real(dr),  ALLOCATABLE :: cbin(:,:)

    end type RasterData


    contains
   !------------------------------------------------------------------------
   subroutine ProcessRaster(FnumTG, Raster_l) !--- Process Raster instructions for this data structure  Raster_l
        implicit none
    
        integer :: FnumTG

        type (RasterData) Raster_l  

        do
            read(FnumTG,'(a)',iostat=status,end=10) Raster_CMD
            call Msg('!-----------------------')
            call Msg('Raster:'//Raster_CMD)
            
            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.Raster'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
            end if
            
            if(index(Raster_CMD, Raster_Read_CMD)  /= 0) then
                call Raster_Read(FnumTG, Raster_l)
                
            else if(index(Raster_CMD, Raster_Write_CMD)  /= 0) then
                call Raster_Write(FnumTG, Raster_l)

            else if(index(Raster_CMD, Raster_ltequals_CMD)  /= 0) then
                call Raster_ltequals(FnumTG, Raster_l)
                call Raster_Bins(Raster_l)

            else if(index(Raster_CMD, Raster_replaceValue_CMD)  /= 0) then
                call Raster_replaceValue(FnumTG, Raster_l)
                call Raster_Bins(Raster_l)

            else if(index(Raster_CMD, Raster_multiplier_CMD)  /= 0) then
                call Raster_multiplier(FnumTG, Raster_l)

            else if(index(Raster_CMD, Raster_XYZto_CMD)  /= 0) then
                call Raster_XYZto(FnumTG, Raster_l)

                
            else if(index(Raster_CMD, Raster_End_CMD)  /= 0) then
                exit
            
            else
                call ErrMsg('Raster?:'//Raster_CMD)
            end if
        end do

        10 continue        
   
    end subroutine ProcessRaster

    !----------------------------------------------------------------------
    subroutine Raster_Read(FnumTG, Raster_l)
	    implicit none
        
        type (RasterData) Raster_l  

        integer :: i, j, k, lpos

        integer :: FnumTG

        character(80) :: line
        integer :: Fnum
        character(MAXLBL) :: FName
        real :: min_elev, max_elev 

        ! Fracman mafic file 
        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Raster file: '//FName)
 
	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'ncols')+5 
	    read(line(lpos:),*) Raster_l.nxrast 
	    write(TmpSTR,*) '# columns: ',Raster_l.nxrast 
        call Msg(TmpSTR)
 
	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'nrows')+5 
	    read(line(lpos:),*) Raster_l.nyrast 
	    write(TmpSTR,*) '# rows:',Raster_l.nyrast
        call Msg(TmpSTR)
 
	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'xllcorner')+9 
	    read(line(lpos:),*) Raster_l.dxmin 
	    write(TmpSTR,*) 'xllcorner:',Raster_l.dxmin
        call Msg(TmpSTR)

	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'yllcorner')+9 
	    read(line(lpos:),*) Raster_l.dymin 
	    write(TmpSTR,*) 'yllcorner:',Raster_l.dymin
        call Msg(TmpSTR)
 
	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'cellsize')+8 
	    read(line(lpos:),*) Raster_l.xspc 
	    Raster_l.yspc=Raster_l.xspc 
	    write(TmpSTR,*) 'cell size:',Raster_l.xspc
        call Msg(TmpSTR)
 
	    read(FNum,'(a)') line 
	    call LwrCse(line) 
	    lpos=index(line,'nodata_value')+12 
	    read(line(lpos:),*) Raster_l.vmiss 
	    write(TmpSTR,*) 'Missing value:',Raster_l.vmiss
        call Msg(TmpSTR)
 
	    if(allocated(Raster_l.rastx)) deallocate(Raster_l.rastx,Raster_l.rasty,Raster_l.rastval) 
	    allocate(Raster_l.rastx(Raster_l.nxrast),Raster_l.rasty(Raster_l.nyrast),Raster_l.rastval(Raster_l.nxrast,Raster_l.nyrast),stat=ialloc) 
	    call AllocChk(ialloc,'Raster arrays') 
 
	    Raster_l.rastx(1)=Raster_l.dxmin
	    do i=2,Raster_l.nxrast
		    Raster_l.rastx(i)=Raster_l.rastx(i-1)+Raster_l.xspc 
	    end do 

	    Raster_l.rasty(1)=Raster_l.dymin
	    do i=2,Raster_l.nyrast
		    Raster_l.rasty(i)=Raster_l.rasty(i-1)+Raster_l.yspc 
	    end do 
 
	    read(FNum,*) ((Raster_l.rastval(j,k),j=1,Raster_l.nxrast),k=Raster_l.nyrast,1,-1) 
 
 
	    min_elev=1.e20 
	    max_elev=-1.e20 
	    do j=1,Raster_l.nxrast 
		    do k=1,Raster_l.nyrast 
			    if(Raster_l.rastval(j,k) /= Raster_l.vmiss) then 
				    if(Raster_l.rastval(j,k) > max_elev) then
                        max_elev=Raster_l.rastval(j,k) 
                        write(*,*) j,k,max_elev
                    end if
				    if(Raster_l.rastval(j,k) < min_elev) min_elev=Raster_l.rastval(j,k) 
			    end if 
		    end do 
	    end do 
	    write(TmpSTR,*) 'Range...' 
        call Msg(TmpSTR)
 
	    write(TmpSTR,*) 'Minimum value:',min_elev
        call Msg(TmpSTR)
	    write(TmpSTR,*) 'Maximum value:',max_elev
        call Msg(TmpSTR)
 
 

	    call freeunit(FNum) 


    end subroutine Raster_Read 
    
    !----------------------------------------------------------------------
    subroutine Raster_Write(FnumTG, Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: FnumTG

        integer :: j, k
        integer :: Fnum
        character(MAXLBL) :: FName

        ! Fracman mafic file 
        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Raster file: '//FName)

    
        write(FNum,'(a,i10)')   'ncols ',        Raster_l.nxrast 
	    write(FNum,'(a,i10)')   'nrows ',        Raster_l.nyrast
	    write(FNum,'(a,e14.7)') 'xllcorner ',    Raster_l.dxmin
        write(FNum,'(a,e14.7)') 'yllcorner ',    Raster_l.dymin
	    write(FNum,'(a,e14.7)') 'cellsize ',     Raster_l.xspc
        write(FNum,'(a,e14.7)') 'nodata_value ', Raster_l.vmiss
 	    do k=Raster_l.nyrast,1,-1
            write(FNum,'(5(1x,e14.7))') (Raster_l.rastval(j,k),j=1,Raster_l.nxrast)
        end do
        
        call freeunit(FNum)

    end subroutine Raster_Write
    !----------------------------------------------------------------------
    subroutine Raster_Bins(Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: i, j, k
        logical :: found

	    if(allocated(Raster_l.vbin)) deallocate(Raster_l.vbin,Raster_l.nbin,Raster_l.cbin) 
	    allocate(Raster_l.vbin(MAXBINS),Raster_l.nbin(MAXBINS),Raster_l.cbin(Raster_l.nxrast,Raster_l.nyrast),stat=ialloc) 
	    call AllocChk(ialloc,'Raster bin arrays') 
        
        
        raster_l.nbinTot=1 
        raster_l.vbin(raster_l.nbinTot)=Raster_l.rastval(1,1) 
        raster_l.cbin(1,1)=1 
        raster_l.nbin(raster_l.nbinTot)=1 

  	    do i=1,Raster_l.nxrast
            do j=1,Raster_l.nyrast 
                found=.false. 
                if(i==2 .and. j==709) then
                    continue
                end if
                do k=1,raster_l.nbinTot
			        if(abs(Raster_l.rastval(i,j)-raster_l.vbin(k)).lt.1.e-37) then 
				        found=.true. 
				        raster_l.cbin(i,j)=k 
				        raster_l.nbin(k)=raster_l.nbin(k)+1 
				        exit
			        end if 
		        end do
                if(.not. found) then 
                    raster_l.nbinTot=raster_l.nbinTot+1 
                    raster_l.vbin(raster_l.nbinTot)=Raster_l.rastval(i,j) 
                    raster_l.cbin(i,j)=raster_l.nbinTot 
                    raster_l.nbin(raster_l.nbinTot)=1 
                end if
            end do
        end do
        
        write(TmpSTR,'(a)') 'Raster values and frequency list:'
        call Msg(TmpSTR(:len_trim(TmpSTR)))
        write(TmpSTR,'(a,i10)') 'Total number of bins (i.e. discrete values): ',raster_l.nbinTot 
        call Msg(TmpSTR(:len_trim(TmpSTR)))
        write(TmpSTR,'(a)') 'Value   frequency'
        call Msg(TmpSTR(:len_trim(TmpSTR)))
        do i=1,raster_l.nbinTot
            write(TmpSTR,'(f20.12,i10)') raster_l.vbin(i),raster_l.nbin(i)
            call Msg(TmpSTR(:len_trim(TmpSTR)))
        end do
        continue

    end subroutine Raster_Bins
    !----------------------------------------------------------------------
    subroutine Raster_LTEquals(FnumTG, Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: FnumTG
        real(dr) :: ltmin, val

        integer :: i, j

        ! Fracman mafic file 
        read(FnumTG,*) ltmin, val

 	    do i=1,Raster_l.nxrast
            do j=1,Raster_l.nyrast 
                if(Raster_l.rastval(i,j) /= Raster_l.vmiss .and. Raster_l.rastval(i,j) <= ltmin) Raster_l.rastval(i,j)=val
            end do
        end do
                    

    end subroutine Raster_LTEquals
    !----------------------------------------------------------------------
    subroutine Raster_ReplaceValue(FnumTG, Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: FnumTG
        real(dr) :: repval, val

        integer :: i, j

        ! Fracman mafic file 
        read(FnumTG,*) repval, val

 	    do i=1,Raster_l.nxrast
            do j=1,Raster_l.nyrast 
                if(Raster_l.rastval(i,j) /= Raster_l.vmiss .and. Raster_l.rastval(i,j) == repval) Raster_l.rastval(i,j)=val
            end do
        end do
                    

    end subroutine Raster_ReplaceValue
    !----------------------------------------------------------------------
    subroutine Raster_Multiplier(FnumTG, Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: FnumTG
        real(dr) :: multiplier

        integer :: i, j

        ! multiplier
        read(FnumTG,*) multiplier

 	    do i=1,Raster_l.nxrast
            do j=1,Raster_l.nyrast 
                Raster_l.rastval(i,j)=Raster_l.rastval(i,j)*multiplier
            end do
        end do
                    

    end subroutine Raster_Multiplier

       !----------------------------------------------------------------------
    subroutine Raster_XYZto(FnumTG, Raster_l)
	    implicit none

        type (RasterData) Raster_l  

        integer :: FnumTG

        integer :: i, j, k
        integer :: Fnum
        character(MAXLBL) :: FName

        integer :: FnumXYZ
        character(MAXLBL) :: FNameXYZ

        character(MAXLBL) :: line
       
        
        integer :: nPts
        real(dr), allocatable :: xt(:)
        real(dr), allocatable :: yt(:)
        real(dr), allocatable :: vt(:)
        logical :: SetnCols

        read(FnumTG,'(a)') FNameXYZ
        call OpenAscii(FnumXYZ,FNameXYZ)
        call Msg( 'XYZ file: '//FNameXYZ)
        
  
        ! count ncols assume x changes then y
        npts=0
        do 
            read(FnumXYZ,'(a)',iostat=status) line
            if(status /= 0) then
                exit
            end if
                
            npts=npts+1
        end do
                
        allocate(xt(npts), yt(npts), vt(npts))
        
        write(*,*) npts
        
        rewind(FnumXYZ)
        SetnCols=.false.
        do i=1,npts
            read(FnumXYZ,*) xt(i), yt(i), vt(i)
            if(i>1 .and. .not. SetnCols) then
                if(yt(i) /= yt(i-1)) then
                    Raster_l.nxrast=i-1
                    SetnCols=.true.
                end if
            end if
        end do

        Raster_l.xspc=xt(Raster_l.nxrast)-xt(Raster_l.nxrast-1)
        Raster_l.nyrast=npts/Raster_l.nxrast 
        Raster_l.dxmin=minval(xt)
        Raster_l.dymin=minval(yt)
        Raster_l.vmiss=-999.
        
        write(*,'(a,i10)')   'ncols ',        Raster_l.nxrast 
	    write(*,'(a,i10)')   'nrows ',        Raster_l.nyrast
	    write(*,'(a,e14.7)') 'xllcorner ',    Raster_l.dxmin
        write(*,'(a,e14.7)') 'yllcorner ',    Raster_l.dymin
        write(*,'(a,e14.7)') 'cellsize ',     Raster_l.xspc
        write(*,'(a,e14.7)') 'nodata_value ', Raster_l.vmiss

        
        if(allocated(Raster_l.rastx)) deallocate(Raster_l.rastx,Raster_l.rasty,Raster_l.rastval) 
	    allocate(Raster_l.rastx(Raster_l.nxrast),Raster_l.rasty(Raster_l.nyrast),Raster_l.rastval(Raster_l.nxrast,Raster_l.nyrast),stat=ialloc) 
	    call AllocChk(ialloc,'Raster arrays') 
 
	    Raster_l.rastx(1)=Raster_l.dxmin
	    do i=2,Raster_l.nxrast
		    Raster_l.rastx(i)=Raster_l.rastx(i-1)+Raster_l.xspc 
        end do 

        Raster_l.yspc =Raster_l.xspc 
	    Raster_l.rasty(1)=Raster_l.dymin
	    do i=2,Raster_l.nyrast
		    Raster_l.rasty(i)=Raster_l.rasty(i-1)+Raster_l.yspc 
	    end do 

        i=1
        do k=Raster_l.nyrast,1,-1
            do j=1,Raster_l.nxrast
                Raster_l.rastval(j,k)=vt(i)
                i=i+1
            end do
        end do


        continue

        FName=trim(FNameXYZ)//'.asc'
        call OpenAscii(Fnum,FName)
        call Msg( 'Raster file: '//FName)

    
        write(FNum,'(a,i10)')   'ncols ',        Raster_l.nxrast 
	    write(FNum,'(a,i10)')   'nrows ',        Raster_l.nyrast
	    write(FNum,'(a,e14.7)') 'xllcorner ',    Raster_l.dxmin
        write(FNum,'(a,e14.7)') 'yllcorner ',    Raster_l.dymin
	    write(FNum,'(a,e14.7)') 'cellsize ',     Raster_l.xspc
        write(FNum,'(a,e14.7)') 'nodata_value ', Raster_l.vmiss
 	    do k=Raster_l.nyrast,1,-1
            write(FNum,'(5(1x,e14.7))') (Raster_l.rastval(j,k),j=1,Raster_l.nxrast)
        end do
        
        call freeunit(FNum)

    end subroutine Raster_XYZto
    !-------------------------------------------------------------------
    subroutine get_raster_value(xp,yp,znew,Raster_l,missing) 
	    implicit none

        type (RasterData) Raster_l  

	    integer :: j, k
	    logical :: foundx, foundy, missing
	    real(dr) :: xp, yp, znew
	    real(dr) :: y1, y2, y3, y4, t, u


	    missing = .false.

	    j=0 
	    foundx=.false. 
	    do while ((j.lt.Raster_l.nxrast-1) .and. (.not. foundx)) 
		    j=j+1 
		    if(xp.ge.Raster_l.rastx(j) .and. xp.le.Raster_l.rastx(j+1)) then 
			    foundx=.true. 
		    end if 
	    end do 
 
	    ! find y row
	    k=0 
	    foundy=.false. 
	    do while ((k.lt.Raster_l.nyrast-1) .and. (.not. foundy)) 
		    k=k+1 
		    if(yp.ge.Raster_l.rasty(k) .and. yp.le.Raster_l.rasty(k+1)) then 
			    foundy=.true. 
		    end if 
	    end do 

	    if(foundx .and. foundy) then 
		    y1=Raster_l.rastval(j,k) 
		    y2=Raster_l.rastval(j+1,k) 
		    y3=Raster_l.rastval(j+1,k+1) 
		    y4=Raster_l.rastval(j,k+1) 
		    if(y1 /= Raster_l.vmiss .and. y2 /= Raster_l.vmiss .and. &
		       y3 /= Raster_l.vmiss .and. y4 /= Raster_l.vmiss) then  ! no missing values, bi-linear interpolation
			    t=(xp-Raster_l.rastx(j))/(Raster_l.rastx(j+1)-Raster_l.rastx(j)) 
			    u=(yp-Raster_l.rasty(k))/(Raster_l.rasty(k+1)-Raster_l.rasty(k)) 
			    znew=(1.0-t)*(1.0-u)*y1 + t*(1.0-u)*y2 + t*u*y3 + (1.0-t)*u*y4 
		    else
			    missing=.true.
		    end if
	    else
		    missing=.true.
	    end if 

    end subroutine get_raster_value


end module Raster

