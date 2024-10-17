Module Materials
    use GeneralRoutines
    implicit none

        integer :: nGWFMaterials
        integer,allocatable :: GWF_MaterialID(:)
        character*256, allocatable :: GWF_MaterialName(:)
        real, allocatable :: Porosity(:)
        real, allocatable :: Kh_Kx(:)
        real, allocatable :: Kv_Kz(:)
        real, allocatable :: Ky(:)
        real, allocatable :: Specificstorage(:)
        real, allocatable :: SpecificYield(:)
        character*256, allocatable :: UnsaturatedFunctionType(:)
        real, allocatable :: Alpha(:)
        real, allocatable :: Beta(:)
        real, allocatable :: Sr(:)
        real, allocatable :: BrooksCoreyExponent(:)   
        character*256, allocatable :: GWF_LengthUnit(:)
        character*256, allocatable :: GWF_TimeUnit(:)
        
        integer :: nCLNMaterials
        integer,allocatable         :: CLN_ID(:)
        character*256, allocatable  :: CLN_Name(:)
        character*256, allocatable  :: CLN_Type(:)
        character*256, allocatable  :: Geometry(:)
        character*256, allocatable  :: Direction(:)
        real, allocatable           :: CircularRadius(:)
        real, allocatable           :: RectangularWidth(:)
        real, allocatable           :: RectangularHeight(:)
        real, allocatable           :: LongitudinalK(:)
        character*256, allocatable  :: FlowTreatment(:)
        character*256, allocatable  :: CLN_LengthUnit(:)
        character*256, allocatable  :: CLN_TimeUnit(:)
        
        integer :: nSWFMaterials
        integer,allocatable :: SWF_MaterialID(:)
        character*256, allocatable :: SWF_MaterialName(:)
        real, allocatable :: ManningCoefficient(:)
        real, allocatable :: DepressionStorageHeight(:)
        real, allocatable :: ObstructionStorageHeight(:)
        real, allocatable :: SWFSmoothingDepth1(:)
        real, allocatable :: SWFSmoothingDepth2(:)
        character*256, allocatable :: SWF_LengthUnit(:)
        character*256, allocatable :: SWF_TimeUnit(:)

        integer :: nET
        integer,allocatable :: ET_ID(:)
        character*256, allocatable :: ET_Name(:)
        character*256, allocatable :: ET_TimeUnit(:)
        real, allocatable :: EvaporationDepth(:)
        real, allocatable :: RootDepth(:)
        character*256, allocatable :: LAI_Table(:)
        real, allocatable :: C1(:)
        real, allocatable :: C2(:)
        real, allocatable :: C3(:)
        real, allocatable :: WiltingPoint(:)
        real, allocatable :: FieldCapacity(:)
        real, allocatable :: OxicLimit(:)
        real, allocatable :: AnoxicLimit(:)
        real, allocatable :: EvaporationMinimum(:)
        real, allocatable :: EvaporationMaximum(:)
        real, allocatable :: CanopyStorageParameter(:)
        real, allocatable :: InitialInterceptionStorage(:)
        character*256, allocatable :: ET_LengthUnit(:)
        
        integer :: nSMS
        integer :: iSMSParameterSet
        integer,allocatable ::          SMS_ID(:)
        character*256, allocatable ::   SMS_Name(:)
        real, allocatable ::            SMS_HCLOSE(:)
        real, allocatable ::            SMS_HICLOSE(:)
        integer, allocatable ::         SMS_MXITER(:)
        integer, allocatable ::         SMS_ITER1(:)
        integer, allocatable ::         SMS_IPRSMS(:)
        integer, allocatable ::         SMS_NONLINMETH(:)
        integer, allocatable ::         SMS_LINMETH(:)
        real, allocatable ::            SMS_THETA(:)
        real, allocatable ::            SMS_KAPPA(:)
        real, allocatable ::            SMS_GAMMA(:)
        real, allocatable ::            SMS_AMOMENTUM(:)
        integer, allocatable ::         SMS_NUMTRACK(:)
        real, allocatable ::            SMS_BTOL(:)
        real, allocatable ::            SMS_BREDUC(:)
        real, allocatable ::            SMS_RES_LIM(:)
        integer, allocatable ::         SMS_ITRUNCNEWTON(:)
        character*256, allocatable ::   SMS_Options(:)
        integer, allocatable ::         SMS_IACL(:)
        integer, allocatable ::         SMS_NORDER(:)
        integer, allocatable ::         SMS_LEVEL(:)
        integer, allocatable ::         SMS_NORTH(:)
        integer, allocatable ::         SMS_IREDSYS(:)
        real, allocatable ::            SMS_RRCTOL(:)
        integer, allocatable ::         SMS_IDROPTOL(:)
        real, allocatable ::            SMS_EPSRN(:)
        character*256, allocatable ::   SMS_LengthUnit(:)
   
    contains
    
    !----------------------------------------------------------------------
    subroutine DB_ReadGWFMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line
        integer :: id

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Find largest material number
        read(itmp,'(a)') line
        nGWFMaterials=0
        do
	        read(itmp,*,iostat=status) id
            if(status/=0) exit
            if(id>nGWFMaterials) nGWFMaterials=id        
        end do
        
        allocate(GWF_MaterialID(nGWFMaterials), & 
            GWF_MaterialName(nGWFMaterials), & 
            Porosity(nGWFMaterials), & 
            Kh_Kx(nGWFMaterials), & 
            Kv_Kz(nGWFMaterials), & 
            Ky(nGWFMaterials), & 
            Specificstorage(nGWFMaterials), & 
            SpecificYield(nGWFMaterials), & 
            UnsaturatedFunctionType(nGWFMaterials), & 
            Alpha(nGWFMaterials), & 
            Beta(nGWFMaterials), & 
            Sr(nGWFMaterials), & 
            BrooksCoreyExponent(nGWFMaterials), & 
            GWF_LengthUnit(nGWFMaterials), &
            GWF_TimeUnit(nGWFMaterials), &
            stat=ialloc)
        call AllocChk(ialloc,'GWF material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nGWFMaterials
            read(itmp,'(a)') line
            
            ! parse line by commas
            GWF_MaterialID(i)           =ParseLineINum(line)
            GWF_MaterialName(i)         =ParseLineSTR(line)
	        Porosity(i)                 =ParseLineRNUM(line)
	        Kh_Kx(i)                    =ParseLineRNUM(line)
            Kv_Kz(i)                    =ParseLineRNUM(line) 
            Ky(i)                       =ParseLineRNUM(line)
            Specificstorage(i)          =ParseLineRNUM(line) 
            SpecificYield(i)            =ParseLineRNUM(line)
            UnsaturatedFunctionType(i)  =ParseLineSTR(line)
            Alpha(i)                    =ParseLineRNUM(line)
            Beta(i)                     =ParseLineRNUM(line)
            Sr(i)                       =ParseLineRNUM(line)
            BrooksCoreyExponent(i)      =ParseLineRNUM(line) 
            GWF_LengthUnit(i)           =ParseLineSTR(line)
            GWF_TimeUnit(i)             =ParseLineSTR(line)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadGWFMaterials
    
    integer function ParseLineINum(line)
        implicit none
        character(*) :: line
        integer :: j
    
        j=INDEX(line,',')
        read(line(:j-1),*) ParseLineINum
        line=line(j+1:)
        
    end function ParseLineINum

    real function ParseLineRNum(line)
        implicit none
        character(*) :: line
        integer :: j
    
        j=INDEX(line,',')
        read(line(:j-1),*) ParseLineRNum
        line=line(j+1:)
        
    end function ParseLineRNum
    
    character(MAX_LBL) function ParseLineSTR(line)
        implicit none
        character(*) :: line
        integer :: j
    
        j=INDEX(line,',')
        read(line(:j-1),'(a)') ParseLineSTR
        line=line(j+1:)
        
    end function ParseLineSTR

    
    !----------------------------------------------------------------------
    subroutine DB_ReadCLNMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line
        integer :: id

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        nCLNMaterials=0
        do
	        read(itmp,*,iostat=status) id
            if(status/=0) exit
            if(id>nCLNMaterials) nCLNMaterials=id        
        end do
        
        allocate(CLN_ID(nCLNMaterials), & 
                 CLN_Name(nCLNMaterials), & 
                 CLN_Type(nCLNMaterials), & 
                 Geometry(nCLNMaterials), & 
                 Direction(nCLNMaterials), & 
                 CircularRadius(nCLNMaterials), & 
                 RectangularWidth(nCLNMaterials), & 
                 RectangularHeight(nCLNMaterials), & 
                 LongitudinalK(nCLNMaterials), & 
                 FlowTreatment(nCLNMaterials), & 
                 CLN_LengthUnit(nCLNMaterials), &
                 CLN_TimeUnit(nCLNMaterials), &
        stat=ialloc)
        call AllocChk(ialloc,'CLN material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nCLNMaterials
            read(itmp,'(a)') line
            
            ! parse line by commas
            CLN_ID(i)           =ParseLineINum(line) 
            CLN_Name(i)         =ParseLineSTR(line) 
            CLN_Type(i)         =ParseLineSTR(line) 
            Geometry(i)         =ParseLineSTR(line)
            Direction(i)        =ParseLineSTR(line)
            CircularRadius(i)   =ParseLineRNUM(line)
            RectangularWidth(i) =ParseLineRNUM(line) 
            RectangularHeight(i)=ParseLineRNUM(line) 
            LongitudinalK(i)    =ParseLineRNUM(line)
            FlowTreatment(i)    =ParseLineSTR(line)
            CLN_LengthUnit(i)   =ParseLineSTR(line)
            CLN_TimeUnit(i)     =ParseLineSTR(line)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadCLNMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadSWFMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line
        integer :: id

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        nSWFMaterials=0
        do
	        read(itmp,*,iostat=status) id
            if(status/=0) exit
            if(id>nSWFMaterials) nSWFMaterials=id        
        end do
        
        allocate(SWF_MaterialID(nSWFMaterials), & 
            SWF_MaterialName(nSWFMaterials), & 
            ManningCoefficient(nSWFMaterials), & 
            DepressionStorageHeight(nSWFMaterials), &
            ObstructionStorageHeight(nSWFMaterials), & 
            SWFSmoothingDepth1(nSWFMaterials), & 
            SWFSmoothingDepth2(nSWFMaterials), &  
            SWF_LengthUnit(nSWFMaterials), &
            SWF_TimeUnit(nSWFMaterials), &
            stat=ialloc)
        call AllocChk(ialloc,'SWF material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nSWFMaterials
            read(itmp,'(a)') line
            
            ! parse line by commas
	        SWF_MaterialID(i)               =ParseLineINum(line) 
            SWF_MaterialName(i)             =ParseLineSTR(line) 
            ManningCoefficient(i)           =ParseLineRNum(line) 
            DepressionStorageHeight(i)      =ParseLineRNum(line)
            ObstructionStorageHeight(i)     =ParseLineRNum(line) 
            SWFSmoothingDepth1(i)           =ParseLineRNum(line)  
            SWFSmoothingDepth2(i)           =ParseLineRNum(line)   
            SWF_LengthUnit(i)               =ParseLineSTR(line)
            SWF_TimeUnit(i)                 =ParseLineSTR(line)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadSWFMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadET(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line
        integer :: id

		call Msg(TAB//TAB//'ET file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        nET=0
        do
	        read(itmp,*,iostat=status) id
            if(status/=0) exit
            if(id>nET) nET=id        
        end do
        
        allocate(ET_ID(nET), & 
            ET_Name(nET), & 
            EvaporationDepth(nET), & 
            RootDepth(nET), &  
            LAI_Table(nET), &  
            C1(nET), & 
            C2(nET), & 
            C3(nET), & 
            WiltingPoint(nET), & 
            FieldCapacity(nET), & 
            OxicLimit(nET), & 
            AnoxicLimit(nET), & 
            EvaporationMinimum(nET), & 
            EvaporationMaximum(nET), & 
            CanopyStorageParameter(nET), & 
            InitialInterceptionStorage(nET), &
            ET_LengthUnit(nET), &
            ET_TimeUnit(nET), &
         stat=ialloc)            
        call AllocChk(ialloc,'ET database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nET
            read(itmp,'(a)') line
            
            ! parse line by commas
	        ET_ID(i)                        =ParseLineINum(line) 
            ET_Name(i)                      =ParseLineSTR(line) 
            EvaporationDepth(i)             =ParseLineRNum(line) 
            RootDepth(i)                    =ParseLineRNum(line)  
            LAI_Table(i)                    =ParseLineSTR(line)  
            C1(i)                           =ParseLineRNum(line) 
            C2(i)                           =ParseLineRNum(line) 
            C3(i)                           =ParseLineRNum(line) 
            WiltingPoint(i)                 =ParseLineRNum(line) 
            FieldCapacity(i)                =ParseLineRNum(line) 
            OxicLimit(i)                    =ParseLineRNum(line) 
            AnoxicLimit(i)                  =ParseLineRNum(line) 
            EvaporationMinimum(i)           =ParseLineRNum(line) 
            EvaporationMaximum(i)           =ParseLineRNum(line) 
            CanopyStorageParameter(i)       =ParseLineRNum(line) 
            InitialInterceptionStorage(i)   =ParseLineRNum(line)
            ET_LengthUnit(i)                =ParseLineSTR(line)
            ET_TimeUnit(i)                  =ParseLineSTR(line)
        end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadET
    !----------------------------------------------------------------------
    subroutine DB_ReadSMS(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line
        integer :: id

		call Msg(TAB//'SMS file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        nSMS=0
        do
	        read(itmp,*,iostat=status) id
            if(status/=0) exit
            if(id>nSMS) nSMS=id        
        end do
        
        allocate(SMS_ID(nSMS), & 
            SMS_Name(nSMS), & 
            SMS_HCLOSE(nSMS), & 
            SMS_HICLOSE(nSMS), & 
            SMS_MXITER(nSMS), & 
            SMS_ITER1(nSMS), & 
            SMS_IPRSMS(nSMS), & 
            SMS_NONLINMETH(nSMS), & 
            SMS_LINMETH(nSMS), & 
            SMS_THETA(nSMS), & 
            SMS_KAPPA(nSMS), & 
            SMS_GAMMA(nSMS), & 
            SMS_AMOMENTUM(nSMS), & 
            SMS_NUMTRACK(nSMS), & 
            SMS_BTOL(nSMS), & 
            SMS_BREDUC(nSMS), & 
            SMS_RES_LIM(nSMS), & 
            SMS_ITRUNCNEWTON(nSMS), & 
            SMS_Options(nSMS), &  
            SMS_IACL(nSMS), &
            SMS_NORDER(nSMS), &
            SMS_LEVEL(nSMS), &
            SMS_NORTH(nSMS), &
            SMS_IREDSYS(nSMS), &
            SMS_RRCTOL(nSMS), &
            SMS_IDROPTOL(nSMS), &
            SMS_EPSRN(nSMS), &
            SMS_LengthUnit(nSMS),&
        stat=ialloc) 
        call AllocChk(ialloc,'SMS database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nSMS
            read(itmp,'(a)') line
            
            ! parse line by commas
            SMS_ID(i)           =ParseLineINum(line) 
            SMS_Name(i)         =ParseLineSTR(line) 
            SMS_HCLOSE(i)       =ParseLineRNum(line) 
            SMS_HICLOSE(i)      =ParseLineRNum(line) 
            SMS_MXITER(i)       =ParseLineINum(line) 
            SMS_ITER1(i)        =ParseLineINum(line) 
            SMS_IPRSMS(i)       =ParseLineINum(line) 
            SMS_NONLINMETH(i)   =ParseLineINum(line) 
            SMS_LINMETH(i)      =ParseLineINum(line) 
            SMS_THETA(i)        =ParseLineRNum(line) 
            SMS_KAPPA(i)        =ParseLineRNum(line) 
            SMS_GAMMA(i)        =ParseLineRNum(line) 
            SMS_AMOMENTUM(i)    =ParseLineRNum(line) 
            SMS_NUMTRACK(i)     =ParseLineINum(line) 
            SMS_BTOL(i)         =ParseLineRNum(line) 
            SMS_BREDUC(i)       =ParseLineRNum(line) 
            SMS_RES_LIM(i)      =ParseLineRNum(line) 
            SMS_ITRUNCNEWTON(i) =ParseLineINum(line) 
            SMS_Options(i)      =ParseLineSTR(line)
            SMS_IACL(i)         =ParseLineINum(line)
            SMS_NORDER(i)       =ParseLineINum(line)
            SMS_LEVEL(i)        =ParseLineINum(line)
            SMS_NORTH(i)        =ParseLineINum(line)
            SMS_IREDSYS(i)      =ParseLineINum(line)
            SMS_RRCTOL(i)       =ParseLineRNum(line)
            SMS_IDROPTOL(i)     =ParseLineINum(line)
            SMS_EPSRN(i)        =ParseLineRNum(line) 
            SMS_LengthUnit(i)   =ParseLineSTR(line)
       end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadSMS

end module Materials
