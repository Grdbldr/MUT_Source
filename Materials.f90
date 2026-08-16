Module Materials
    use GeneralRoutines
    use ErrorHandling, only: ERR_FILE_IO, HandleError
    implicit none

        integer(i4) :: nGWFMaterials
        integer(i4),allocatable :: GWF_MaterialID(:)
        character*256, allocatable :: GWF_MaterialName(:)
        real(sp), allocatable :: Porosity(:)
        real(sp), allocatable :: Kh_Kx(:)
        real(sp), allocatable :: Kv_Kz(:)
        real(sp), allocatable :: Ky(:)
        real(sp), allocatable :: Specificstorage(:)
        real(sp), allocatable :: SpecificYield(:)
        character*256, allocatable :: UnsaturatedFunctionType(:)
        real(sp), allocatable :: Alpha(:)
        real(sp), allocatable :: Beta(:)
        real(sp), allocatable :: Sr(:)
        real(sp), allocatable :: BrooksCoreyExponent(:)   
        character*256, allocatable :: GWF_LengthUnit(:)
        character*256, allocatable :: GWF_TimeUnit(:)
        
        integer(i4) :: nCLNMaterials
        integer(i4),allocatable         :: CLN_ID(:)
        character*256, allocatable  :: CLN_Name(:)
        character*256, allocatable  :: CLN_Type(:)
        character*256, allocatable  :: Geometry(:)
        character*256, allocatable  :: Direction(:)
        real(sp), allocatable           :: CircularRadius(:)
        real(sp), allocatable           :: RectangularWidth(:)
        real(sp), allocatable           :: RectangularHeight(:)
        character*256, allocatable  :: GeneralSectionTableFile(:)
        real(sp), allocatable           :: LongitudinalK(:)
        character*256, allocatable  :: FlowTreatment(:)
        character*256, allocatable  :: CLN_LengthUnit(:)
        character*256, allocatable  :: CLN_TimeUnit(:)
        real(sp), allocatable           :: InfillPorosity(:)
        
        integer(i4) :: nSWFMaterials
        integer(i4),allocatable :: SWF_MaterialID(:)
        character*256, allocatable :: SWF_MaterialName(:)
        real(sp), allocatable :: ManningCoefficient(:)
        real(sp), allocatable :: DepressionStorageHeight(:)
        real(sp), allocatable :: ObstructionStorageHeight(:)
        real(sp), allocatable :: SWFSmoothingDepth1(:)
        real(sp), allocatable :: SWFSmoothingDepth2(:)
        character*256, allocatable :: SWF_LengthUnit(:)
        character*256, allocatable :: SWF_TimeUnit(:)

        integer(i4) :: nET
        integer(i4),allocatable :: ET_ID(:)
        character*256, allocatable :: ET_Name(:)
        character*256, allocatable :: ET_TimeUnit(:)
        real(sp), allocatable :: EvaporationDepth(:)
        real(sp), allocatable :: RootDepth(:)
        character*256, allocatable :: LAI_Table(:)
        real(sp), allocatable :: C1(:)
        real(sp), allocatable :: C2(:)
        real(sp), allocatable :: C3(:)
        real(sp), allocatable :: WiltingPoint(:)
        real(sp), allocatable :: FieldCapacity(:)
        real(sp), allocatable :: OxicLimit(:)
        real(sp), allocatable :: AnoxicLimit(:)
        real(sp), allocatable :: EvaporationMinimum(:)
        real(sp), allocatable :: EvaporationMaximum(:)
        real(sp), allocatable :: CanopyStorageParameter(:)
        real(sp), allocatable :: InitialInterceptionStorage(:)
        character*256, allocatable :: ET_LengthUnit(:)
        
        integer(i4) :: nSMS
        integer(i4) :: iSMSParameterSet
        integer(i4),allocatable ::          SMS_ID(:)
        character*256, allocatable ::   SMS_Name(:)
        real(sp), allocatable ::            SMS_HCLOSE(:)
        real(sp), allocatable ::            SMS_HICLOSE(:)
        integer(i4), allocatable ::         SMS_MXITER(:)
        integer(i4), allocatable ::         SMS_ITER1(:)
        integer(i4), allocatable ::         SMS_IPRSMS(:)
        integer(i4), allocatable ::         SMS_NONLINMETH(:)
        integer(i4), allocatable ::         SMS_LINMETH(:)
        real(sp), allocatable ::            SMS_THETA(:)
        real(sp), allocatable ::            SMS_KAPPA(:)
        real(sp), allocatable ::            SMS_GAMMA(:)
        real(sp), allocatable ::            SMS_AMOMENTUM(:)
        integer(i4), allocatable ::         SMS_NUMTRACK(:)
        real(sp), allocatable ::            SMS_BTOL(:)
        real(sp), allocatable ::            SMS_BREDUC(:)
        real(sp), allocatable ::            SMS_RES_LIM(:)
        integer(i4), allocatable ::         SMS_ITRUNCNEWTON(:)
        character*256, allocatable ::   SMS_Options(:)
        integer(i4), allocatable ::         SMS_IACL(:)
        integer(i4), allocatable ::         SMS_NORDER(:)
        integer(i4), allocatable ::         SMS_LEVEL(:)
        integer(i4), allocatable ::         SMS_NORTH(:)
        integer(i4), allocatable ::         SMS_IREDSYS(:)
        real(sp), allocatable ::            SMS_RRCTOL(:)
        integer(i4), allocatable ::         SMS_IDROPTOL(:)
        real(sp), allocatable ::            SMS_EPSRN(:)
        character*256, allocatable ::   SMS_LengthUnit(:)
   
    contains
    
    !----------------------------------------------------------------------
    subroutine DB_ReadGWFMaterials(FName)
        implicit none

	    character(*) :: FName
	    character(256) :: line
        integer(i4) :: id

		call Msg('Materials file '//trim(FName))
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FName), 'DB_ReadGWFMaterials')
        end if
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

        GWF_MaterialID = 0
        rewind(itmp)
        read(itmp,'(a)') line
        ! Store by material ID so sparse IDs (gaps) are allowed; do not assume one row per ID
        do
            read(itmp,'(a)',iostat=status) line
            if(status/=0) exit
            if(len_trim(line)==0) cycle

            id = ParseLineINum(line)
            if(id < 1 .or. id > nGWFMaterials) then
                call HandleError(ERR_FILE_IO, 'Invalid GWF material ID in: '//trim(FName), 'DB_ReadGWFMaterials')
            end if

            GWF_MaterialID(id)           = id
            GWF_MaterialName(id)         =ParseLineSTR(line)
	        Porosity(id)                 =ParseLineRNUM(line)
	        Kh_Kx(id)                    =ParseLineRNUM(line)
            Kv_Kz(id)                    =ParseLineRNUM(line) 
            Ky(id)                       =ParseLineRNUM(line)
            Specificstorage(id)          =ParseLineRNUM(line) 
            SpecificYield(id)            =ParseLineRNUM(line)
            UnsaturatedFunctionType(id)  =ParseLineSTR(line)
            Alpha(id)                    =ParseLineRNUM(line)
            Beta(id)                     =ParseLineRNUM(line)
            Sr(id)                       =ParseLineRNUM(line)
            BrooksCoreyExponent(id)      =ParseLineRNUM(line) 
            GWF_LengthUnit(id)           =ParseLineSTR(line)
            GWF_TimeUnit(id)             =ParseLineSTR(line)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadGWFMaterials
    
    integer(i4) function ParseLineINum(line)
        implicit none
        character(*) :: line
        integer(i4) :: j
    
        if(len_trim(line)==0) then
            ParseLineINum = 0
            return
        end if
        j=INDEX(line,',')
        if(j==0) then
            read(line,*) ParseLineINum
            line=''
        else
            read(line(:j-1),*) ParseLineINum
            line=line(j+1:)
        end if
        
    end function ParseLineINum

    real(sp) function ParseLineRNum(line)
        implicit none
        character(*) :: line
        integer(i4) :: j
    
        if(len_trim(line)==0) then
            ParseLineRNum = 0.0
            return
        end if
        j=INDEX(line,',')
        if(j==0) then
            read(line,*) ParseLineRNum
            line=''
        else
            read(line(:j-1),*) ParseLineRNum
            line=line(j+1:)
        end if
        
    end function ParseLineRNum
    
    character(MAX_LBL) function ParseLineSTR(line)
        implicit none
        character(*) :: line
        integer(i4) :: j
    
        if(len_trim(line)==0) then
            ParseLineSTR = ''
            return
        end if
        j=INDEX(line,',')
        if(j==0) then
            read(line,'(a)') ParseLineSTR
            line=''
        else
            read(line(:j-1),'(a)') ParseLineSTR
            line=line(j+1:)
        end if
        
    end function ParseLineSTR

    
    !----------------------------------------------------------------------
    subroutine DB_ReadCLNMaterials(FName)
        implicit none

	    character(*) :: FName
	    character(256) :: line
        integer(i4) :: id

		call Msg('Materials file '//trim(FName))
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FName), 'DB_ReadCLNMaterials')
        end if
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
                 GeneralSectionTableFile(nCLNMaterials), &
                 LongitudinalK(nCLNMaterials), & 
                 FlowTreatment(nCLNMaterials), & 
                 CLN_LengthUnit(nCLNMaterials), &
                 CLN_TimeUnit(nCLNMaterials), &
                 InfillPorosity(nCLNMaterials), &
        stat=ialloc)
        call AllocChk(ialloc,'CLN material database arrays')

        CLN_ID = 0
        InfillPorosity = 1.0
        rewind(itmp)
        read(itmp,'(a)') line
        ! Store by material ID so sparse IDs (gaps) are allowed; do not assume one row per ID
        do
            read(itmp,'(a)',iostat=status) line
            if(status/=0) exit
            if(len_trim(line)==0) cycle

            id = ParseLineINum(line)
            if(id < 1 .or. id > nCLNMaterials) then
                call HandleError(ERR_FILE_IO, 'Invalid CLN material ID in: '//trim(FName), 'DB_ReadCLNMaterials')
            end if

            CLN_ID(id)           = id
            CLN_Name(id)         =ParseLineSTR(line) 
            CLN_Type(id)         =ParseLineSTR(line) 
            Geometry(id)         =ParseLineSTR(line)
            Direction(id)        =ParseLineSTR(line)
            CircularRadius(id)   =ParseLineRNUM(line)
            RectangularWidth(id) =ParseLineRNUM(line) 
            RectangularHeight(id)=ParseLineRNUM(line)
            LongitudinalK(id)    =ParseLineRNUM(line)
            FlowTreatment(id)    =ParseLineSTR(line)
            CLN_LengthUnit(id)   =ParseLineSTR(line)
            CLN_TimeUnit(id)     =ParseLineSTR(line)
            ! Optional trailing fields (backward compatible):
            ! General: GeneralSectionTableFile [, InfillPorosity]
            ! Other geometries: [InfillPorosity] (numeric only)
            GeneralSectionTableFile(id)=''
            InfillPorosity(id)=1.0
            if(len_trim(line) > 0) then
                if(trim(Geometry(id)) == 'General') then
                    GeneralSectionTableFile(id)=ParseLineSTR(line)
                    if(len_trim(line) > 0) then
                        InfillPorosity(id)=ParseLineRNUM(line)
                        if(InfillPorosity(id) <= 0.0) InfillPorosity(id)=1.0
                    end if
                else
                    InfillPorosity(id)=ParseLineRNUM(line)
                    if(InfillPorosity(id) <= 0.0) InfillPorosity(id)=1.0
                end if
            end if
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadCLNMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadSWFMaterials(FName)
        implicit none

	    character(*) :: FName
	    character(256) :: line
        integer(i4) :: id

		call Msg('Materials file '//trim(FName))
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FName), 'DB_ReadSWFMaterials')
        end if
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

        SWF_MaterialID = 0
        rewind(itmp)
        read(itmp,'(a)') line
        ! Store by material ID so sparse IDs (gaps) are allowed; do not assume one row per ID
        do
            read(itmp,'(a)',iostat=status) line
            if(status/=0) exit
            if(len_trim(line)==0) cycle

            id = ParseLineINum(line)
            if(id < 1 .or. id > nSWFMaterials) then
                call HandleError(ERR_FILE_IO, 'Invalid SWF material ID in: '//trim(FName), 'DB_ReadSWFMaterials')
            end if

	        SWF_MaterialID(id)               = id
            SWF_MaterialName(id)             =ParseLineSTR(line) 
            ManningCoefficient(id)           =ParseLineRNum(line) 
            DepressionStorageHeight(id)      =ParseLineRNum(line)
            ObstructionStorageHeight(id)     =ParseLineRNum(line) 
            SWFSmoothingDepth1(id)           =ParseLineRNum(line)  
            SWFSmoothingDepth2(id)           =ParseLineRNum(line)   
            SWF_LengthUnit(id)               =ParseLineSTR(line)
            SWF_TimeUnit(id)                 =ParseLineSTR(line)
        end do
        
        call freeunit(itmp)

    end subroutine DB_ReadSWFMaterials
    
    !----------------------------------------------------------------------
    subroutine DB_ReadET(FName)
        implicit none

	    character(*) :: FName
	    character(256) :: line
        integer(i4) :: id

		call Msg('ET file '//trim(FName))
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FName), 'DB_ReadET')
        end if
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

        ET_ID = 0
        rewind(itmp)
        read(itmp,'(a)') line
        ! Store by ET ID so sparse IDs (gaps) are allowed; do not assume one row per ID
        do
            read(itmp,'(a)',iostat=status) line
            if(status/=0) exit
            if(len_trim(line)==0) cycle

            id = ParseLineINum(line)
            if(id < 1 .or. id > nET) then
                call HandleError(ERR_FILE_IO, 'Invalid ET ID in: '//trim(FName), 'DB_ReadET')
            end if

	        ET_ID(id)                        = id
            ET_Name(id)                      =ParseLineSTR(line) 
            EvaporationDepth(id)             =ParseLineRNum(line) 
            RootDepth(id)                    =ParseLineRNum(line)  
            LAI_Table(id)                    =ParseLineSTR(line)  
            C1(id)                           =ParseLineRNum(line) 
            C2(id)                           =ParseLineRNum(line) 
            C3(id)                           =ParseLineRNum(line) 
            WiltingPoint(id)                 =ParseLineRNum(line) 
            FieldCapacity(id)                =ParseLineRNum(line) 
            OxicLimit(id)                    =ParseLineRNum(line) 
            AnoxicLimit(id)                  =ParseLineRNum(line) 
            EvaporationMinimum(id)           =ParseLineRNum(line) 
            EvaporationMaximum(id)           =ParseLineRNum(line) 
            CanopyStorageParameter(id)       =ParseLineRNum(line) 
            InitialInterceptionStorage(id)   =ParseLineRNum(line)
            ET_LengthUnit(id)                =ParseLineSTR(line)
            ET_TimeUnit(id)                  =ParseLineSTR(line)
        end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadET
    !----------------------------------------------------------------------
    subroutine DB_ReadSMS(FName)
        implicit none

	    character(*) :: FName
	    character(256) :: line
        integer(i4) :: id

		call Msg('SMS file '//trim(FName))
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FName), 'DB_ReadSMS')
        end if
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

        SMS_ID = 0
        rewind(itmp)
        read(itmp,'(a)') line
        ! Store by SMS ID so sparse IDs (gaps) are allowed; do not assume one row per ID
        do
            read(itmp,'(a)',iostat=status) line
            if(status/=0) exit
            if(len_trim(line)==0) cycle

            id = ParseLineINum(line)
            if(id < 1 .or. id > nSMS) then
                call HandleError(ERR_FILE_IO, 'Invalid SMS ID in: '//trim(FName), 'DB_ReadSMS')
            end if

            SMS_ID(id)           = id
            SMS_Name(id)         =ParseLineSTR(line) 
            SMS_HCLOSE(id)       =ParseLineRNum(line) 
            SMS_HICLOSE(id)      =ParseLineRNum(line) 
            SMS_MXITER(id)       =ParseLineINum(line) 
            SMS_ITER1(id)        =ParseLineINum(line) 
            SMS_IPRSMS(id)       =ParseLineINum(line) 
            SMS_NONLINMETH(id)   =ParseLineINum(line) 
            SMS_LINMETH(id)      =ParseLineINum(line) 
            SMS_THETA(id)        =ParseLineRNum(line) 
            SMS_KAPPA(id)        =ParseLineRNum(line) 
            SMS_GAMMA(id)        =ParseLineRNum(line) 
            SMS_AMOMENTUM(id)    =ParseLineRNum(line) 
            SMS_NUMTRACK(id)     =ParseLineINum(line) 
            SMS_BTOL(id)         =ParseLineRNum(line) 
            SMS_BREDUC(id)       =ParseLineRNum(line) 
            SMS_RES_LIM(id)      =ParseLineRNum(line) 
            SMS_ITRUNCNEWTON(id) =ParseLineINum(line) 
            SMS_Options(id)      =ParseLineSTR(line)
            SMS_IACL(id)         =ParseLineINum(line)
            SMS_NORDER(id)       =ParseLineINum(line)
            SMS_LEVEL(id)        =ParseLineINum(line)
            SMS_NORTH(id)        =ParseLineINum(line)
            SMS_IREDSYS(id)      =ParseLineINum(line)
            SMS_RRCTOL(id)       =ParseLineRNum(line)
            SMS_IDROPTOL(id)     =ParseLineINum(line)
            SMS_EPSRN(id)        =ParseLineRNum(line) 
            SMS_LengthUnit(id)   =ParseLineSTR(line)
       end do
        
        
        call freeunit(itmp)

    end subroutine DB_ReadSMS

end module Materials
