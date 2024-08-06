Module Materials
    use GeneralRoutines
    implicit none

        integer :: nGWFMaterials
        integer,allocatable :: GWFMaterialID(:)
        character*256, allocatable :: GWFMaterialName(:)
        real, allocatable :: Porosity(:)
        real, allocatable :: Kh_Kx(:)
        real, allocatable :: Kv_Kz(:)
        real, allocatable :: Ky(:)
        real, allocatable :: Specificstorage(:)
        real, allocatable :: SpecificYield(:)
        integer, allocatable :: Brooks_Corey(:)
        real, allocatable :: Alpha(:)
        real, allocatable :: Beta(:)
        real, allocatable :: Sr(:)
        integer, allocatable :: Tabular_Input(:)
        character*256, allocatable :: PSTable(:)
        character*256, allocatable :: SKrTable(:)
        
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
        
        integer :: nSWFMaterials
        integer,allocatable :: SWFMaterialID(:)
        character*256, allocatable :: SWFMaterialName(:)
        real, allocatable :: ManningCoefficient(:)
        real, allocatable :: DepressionStorageHeight(:)
        real, allocatable :: ObstructionStorageHeight(:)
        real, allocatable :: SWFSmoothingDepth1(:)
        real, allocatable :: SWFSmoothingDepth2(:)

        integer :: nET
        integer,allocatable :: ET_ID(:)
        character*256, allocatable :: ET_Name(:)
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
   
    contains
    
    !----------------------------------------------------------------------
    subroutine DB_ReadGWFMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        i=0
        do
	        read(itmp,*,iostat=status) line
            if(status/=0) exit
            i=i+1        
        end do
        
        nGWFMaterials=i
        
        allocate(GWFMaterialID(nGWFMaterials), & 
            GWFMaterialName(nGWFMaterials), & 
            Porosity(nGWFMaterials), & 
            Kh_Kx(nGWFMaterials), & 
            Kv_Kz(nGWFMaterials), & 
            Ky(nGWFMaterials), & 
            Specificstorage(nGWFMaterials), & 
            SpecificYield(nGWFMaterials), & 
            Brooks_Corey(nGWFMaterials), & 
            Alpha(nGWFMaterials), & 
            Beta(nGWFMaterials), & 
            Sr(nGWFMaterials), & 
            Tabular_Input(nGWFMaterials), & 
            PSTable(nGWFMaterials), & 
            SKrTable(nGWFMaterials), & 
            stat=ialloc)
        call AllocChk(ialloc,'GWF material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nGWFMaterials
	        read(itmp,*,iostat=status) GWFMaterialID(i), &
                GWFMaterialName(i), & 
                Porosity(i), & 
                Kh_Kx(i), & 
                Kv_Kz(i), & 
                Ky(i), & 
                Specificstorage(i), & 
                SpecificYield(i), & 
                Brooks_Corey(i), & 
                Alpha(i), & 
                Beta(i), & 
                Sr(i), & 
                Tabular_Input(i), & 
                PSTable(i), & 
                SKrTable(i)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadGWFMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadCLNMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        i=0
        do
	        read(itmp,*,iostat=status) line
            if(status/=0) exit
            i=i+1        
        end do
        
        nCLNMaterials=i
        
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
        stat=ialloc)
        call AllocChk(ialloc,'CLN material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nCLNMaterials
	        read(itmp,*,iostat=status)  CLN_ID(i), & 
                                        CLN_Name(i), & 
                                        CLN_Type(i), & 
                                        Geometry(i), &
                                        Direction(i), &
                                        CircularRadius(i), &
                                        RectangularWidth(i), & 
                                        RectangularHeight(i), & 
                                        LongitudinalK(i), &
                                        FlowTreatment(i)
            continue
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadCLNMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadSWFMaterials(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line

		call Msg(TAB//'Materials file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        i=0
        do
	        read(itmp,*,iostat=status) line
            if(status/=0) exit
            i=i+1        
        end do
        
        nSWFMaterials=i
        
        allocate(SWFMaterialID(nSWFMaterials), & 
            SWFMaterialName(nSWFMaterials), & 
            ManningCoefficient(nSWFMaterials), & 
            DepressionStorageHeight(nSWFMaterials), &
            ObstructionStorageHeight(nSWFMaterials), & 
            SWFSmoothingDepth1(nSWFMaterials), & 
            SWFSmoothingDepth2(nSWFMaterials), &  
            stat=ialloc)
        call AllocChk(ialloc,'SWF material database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nSWFMaterials
	        read(itmp,*,iostat=status) SWFMaterialID(i), & 
                SWFMaterialName(i), & 
                ManningCoefficient(i), & 
                DepressionStorageHeight(i), &
                ObstructionStorageHeight(i), & 
                SWFSmoothingDepth1(i), &  
                SWFSmoothingDepth2(i)   
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadSWFMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadET(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line

		call Msg(TAB//TAB//'ET file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        i=0
        do
	        read(itmp,*,iostat=status) line
            if(status/=0) exit
            i=i+1        
        end do
        
        nET=i
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
            InitialInterceptionStorage(nET), stat=ialloc)
        call AllocChk(ialloc,'ET database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nET
	        read(itmp,*,iostat=status) ET_ID(i), & 
                ET_Name(i), & 
                EvaporationDepth(i), & 
                RootDepth(i), &  
                LAI_Table(i), &  
                C1(i), & 
                C2(i), & 
                C3(i), & 
                WiltingPoint(i), & 
                FieldCapacity(i), & 
                OxicLimit(i), & 
                AnoxicLimit(i), & 
                EvaporationMinimum(i), & 
                EvaporationMaximum(i), & 
                CanopyStorageParameter(i), & 
                InitialInterceptionStorage(i)
        end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadET
    !----------------------------------------------------------------------
    subroutine DB_ReadSMS(FName)
        implicit none

        
        integer :: i

	    character(*) :: FName
	    character(256) :: line

		call Msg(TAB//'SMS file '//trim(FName))
        call OpenAscii(itmp,FName)
	    
        ! Count materials
        read(itmp,'(a)') line
        i=0
        do
	        read(itmp,*,iostat=status) line
            if(status/=0) exit
            i=i+1        
        end do
        
        nSMS=i
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
            SMS_EPSRN(nSMS),stat=ialloc)
        call AllocChk(ialloc,'SMS database arrays')

        rewind(itmp)
        read(itmp,'(a)') line
        do i=1,nSMS
	        read(itmp,*,iostat=status)  SMS_ID(i), & 
                SMS_Name(i), & 
                SMS_HCLOSE(i), & 
                SMS_HICLOSE(i), & 
                SMS_MXITER(i), & 
                SMS_ITER1(i), & 
                SMS_IPRSMS(i), & 
                SMS_NONLINMETH(i), & 
                SMS_LINMETH(i), & 
                SMS_THETA(i), & 
                SMS_KAPPA(i), & 
                SMS_GAMMA(i), & 
                SMS_AMOMENTUM(i), & 
                SMS_NUMTRACK(i), & 
                SMS_BTOL(i), & 
                SMS_BREDUC(i), & 
                SMS_RES_LIM(i), & 
                SMS_ITRUNCNEWTON(i), & 
                SMS_Options(i), &
                SMS_IACL(i), &
                SMS_NORDER(i), &
                SMS_LEVEL(i), &
                SMS_NORTH(i), &
                SMS_IREDSYS(i), &
                SMS_RRCTOL(i), &
                SMS_IDROPTOL(i), &
                SMS_EPSRN(i)
        end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadSMS

end module Materials
