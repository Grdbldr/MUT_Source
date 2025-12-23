module ArrayUtilities
    !### Array management utilities
    ! Provides routines for dynamic array growth and allocation checking
    
    use KindParameters
    use GeneralRoutines, only: MAX_STR, ErrFNum, Msg, ialloc
    
    implicit none
    private
    
    public :: AllocChk, GrowLogicalArray, GrowIntegerArray, GrowInteger2dArray
    public :: GrowRealArray, GrowReal2dArray, GrowDRealArray, GrowDReal2dArray
    
    contains
    
    !----------------------------------------------------------------------
    subroutine AllocChk(ialloc,string) 
        ! Array allocation error message to file and display. Stop
        implicit none
        character*(*) string
	 	integer(i4) ialloc

        if(ialloc.ne.0) then
            write(*,*) 'error: allocating (run-time error number: ',ialloc,')'
            write(*,*) string
			write(ErrFNum,*) 'error: allocating (run-time error number: ',ialloc,')'
			write(ErrFNum,*) string
            stop ! array allocation
        end if

    end subroutine AllocChk
    
    !----------------------------------------------------------------------
    subroutine GrowLogicalArray(iArray,nSizeIn,nSizeout)
        implicit none
        logical, allocatable :: iArray(:)
        logical, allocatable :: iTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowLogicalArray')    
            return
        endif
        
        if(.not. allocated(iArray)) then
            allocate(iArray(nSizeout),stat=ialloc)
            return
        endif

        allocate(iTMP(nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowLogicalArray iTMP array')
        iTMP(1:nSizeIn) = iArray
        call move_alloc (iTMP, iArray)
        
    end subroutine GrowLogicalArray

    !----------------------------------------------------------------------
    subroutine GrowIntegerArray(iArray,nSizeIn,nSizeout)
        implicit none
        integer(i4), allocatable :: iArray(:)
        integer(i4), allocatable :: iTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowIntegerArray')    
            return
        endif
        
        if(.not. allocated(iArray)) then
            allocate(iArray(nSizeout),stat=ialloc)
            return
        endif

        allocate(iTMP(nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowIntegerArray iTMP array')
        iTMP(1:nSizeIn) = iArray
        call move_alloc (iTMP, iArray)
        
    end subroutine GrowIntegerArray
    
    !----------------------------------------------------------------------
    subroutine GrowInteger2dArray(iArray,nSize1,nSizeIn,nSizeout)
        implicit none
        integer(i4), allocatable :: iArray(:,:)
        integer(i4), allocatable :: iTMP(:,:)
        integer(i4) :: nSize1, nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowInteger2dArray')    
            return
        endif
        
        if(.not. allocated(iArray)) then
            allocate(iArray(nSize1,nSizeout),stat=ialloc)
            return
        endif

        allocate(iTMP(nSize1,nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowInteger2dArray iTMP array')
        iTMP (:,1:nSizeIn) = iArray
        call move_alloc (iTMP, iArray)
        
    end subroutine GrowInteger2dArray
    
    !----------------------------------------------------------------------
    subroutine GrowRealArray(rArray,nSizeIn,nSizeout)
        implicit none
        real(sp), allocatable :: rArray(:)
        real(sp), allocatable :: rTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowRealArray')    
            return
        endif
        
        if(.not. allocated(rArray)) then
            allocate(rArray(nSizeout),stat=ialloc)
            return
        endif

        allocate(rTMP(nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowRealArray rTMP array')
        rTMP (1:nSizeIn) = rArray
        call move_alloc (rTMP, rArray)
        
    end subroutine GrowRealArray
    
    !----------------------------------------------------------------------
    subroutine GrowReal2dArray(rArray,nSize1,nSizeIn,nSizeout)
        implicit none
        real(sp), allocatable :: rArray(:,:)
        real(sp), allocatable :: rTMP(:,:)
        integer(i4) :: nSize1, nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowReal2dArray')    
            return
        endif
        
        if(.not. allocated(rArray)) then
            allocate(rArray(nSize1,nSizeout),stat=ialloc)
            return
        endif

        allocate(rTMP(nSize1,nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowReal2dArray rTMP array')
        rTMP (:,1:nSizeIn) = rArray
        call move_alloc (rTMP, rArray)
        
    end subroutine GrowReal2dArray

    !----------------------------------------------------------------------
    subroutine GrowDRealArray(rArray,nSizeIn,nSizeout)
        implicit none
        real(dp), allocatable :: rArray(:)
        real(dp), allocatable :: rTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowDRealArray')    
            return
        endif
        
        if(.not. allocated(rArray)) then
            allocate(rArray(nSizeout),stat=ialloc)
            return
        endif

        allocate(rTMP(nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowDRealArray rTMP array')
        rTMP (1:nSizeIn) = rArray
        call move_alloc (rTMP, rArray)
        
    end subroutine GrowDRealArray
    
    !----------------------------------------------------------------------
    subroutine GrowDReal2dArray(rArray,nSize1,nSizeIn,nSizeout)
        implicit none
        real(dp), allocatable :: rArray(:,:)
        real(dp), allocatable :: rTMP(:,:)
        integer(i4) :: nSize1, nSizeIn, nSizeOut
        
        if(nSizeout<nSizeIn) then
            call Msg('Requested size less than current size: in GrowDReal2dArray')    
            return
        endif
        
        if(.not. allocated(rArray)) then
            allocate(rArray(nSize1,nSizeout),stat=ialloc)
            return
        endif

        allocate(rTMP(nSize1,nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowDReal2dArray rTMP array')
        rTMP (:,1:nSizeIn) = rArray
        call move_alloc (rTMP, rArray)
        
    end subroutine GrowDReal2dArray

end module ArrayUtilities

