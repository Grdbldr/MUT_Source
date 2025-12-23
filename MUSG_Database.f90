module MUSG_Database
    !### Database management for MODFLOW-USG
    ! Handles loading and management of material property databases (SMS, GWF, CLN, SWF, ET)
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, LocalUserbin, LocalUserbinPath, DefineUserbin, USERBIN, Msg
    use GeneralRoutines, only: FMT_R4, FMT_R8, TmpSTR, UnitsOfLength, LengthConverter
    use Materials, only: DB_ReadSMS, DB_ReadGWFMaterials, DB_ReadCLNMaterials, DB_ReadSWFMaterials, DB_ReadET
    use Materials, only: iSMSParameterSet, SMS_Name, SMS_HCLOSE, SMS_HICLOSE, SMS_MXITER, SMS_ITER1
    use Materials, only: SMS_IPRSMS, SMS_NONLINMETH, SMS_LINMETH, SMS_THETA, SMS_KAPPA, SMS_GAMMA
    use Materials, only: SMS_AMOMENTUM, SMS_NUMTRACK, SMS_BTOL, SMS_BREDUC, SMS_RES_LIM, SMS_ITRUNCNEWTON
    use Materials, only: SMS_Options, SMS_IACL, SMS_NORDER, SMS_LEVEL, SMS_NORTH, SMS_IREDSYS
    use Materials, only: SMS_RRCTOL, SMS_IDROPTOL, SMS_EPSRN, SMS_LengthUnit
    use GeneralRoutines, only: MUTVersion
    
    implicit none
    private
    
    public :: LocalUserbin_CMD, SMS_Database_CMD, GWFMaterialsDatabase_CMD
    public :: CLNMaterialsDatabase_CMD, SWFMaterialsDatabase_CMD, ET_Database_CMD
    public :: SMSParamterSetNumber_CMD
    public :: LoadSMSDatabase, LoadGWFMaterialsDatabase, LoadCLNMaterialsDatabase
    public :: LoadSWFMaterialsDatabase, LoadETDatabase, SetSMSParameterSet
    public :: ProcessDatabaseInstruction
    
    ! Database command strings
    character(MAX_INST), parameter :: LocalUserbin_CMD = 'use local databases'
    character(MAX_INST), parameter :: SMS_Database_CMD = 'sms database'
    character(MAX_INST), parameter :: GWFMaterialsDatabase_CMD = 'gwf materials database'
    character(MAX_INST), parameter :: CLNMaterialsDatabase_CMD = 'cln materials database'
    character(MAX_INST), parameter :: SWFMaterialsDatabase_CMD = 'swf materials database'
    character(MAX_INST), parameter :: ET_Database_CMD = 'et database'
    character(MAX_INST), parameter :: SMSParamterSetNumber_CMD = 'sms parameter set number'
    
    contains
    
    !----------------------------------------------------------------------
    subroutine LoadSMSDatabase(FName)
        ! Load SMS solver parameter database
        implicit none
        character(*) :: FName
        
        call DefineUserbin(USERBIN)
        call DB_ReadSMS(trim(USERBIN)//'\'//trim(FName))
    end subroutine LoadSMSDatabase
    
    !----------------------------------------------------------------------
    subroutine LoadGWFMaterialsDatabase(FName)
        ! Load GWF materials database
        implicit none
        character(*) :: FName
        
        call DefineUserbin(USERBIN)
        call DB_ReadGWFMaterials(trim(USERBIN)//'\'//trim(FName))
    end subroutine LoadGWFMaterialsDatabase
    
    !----------------------------------------------------------------------
    subroutine LoadCLNMaterialsDatabase(FName)
        ! Load CLN materials database
        implicit none
        character(*) :: FName
        
        call DefineUserbin(USERBIN)
        call DB_ReadCLNMaterials(trim(USERBIN)//'\'//trim(FName))
    end subroutine LoadCLNMaterialsDatabase
    
    !----------------------------------------------------------------------
    subroutine LoadSWFMaterialsDatabase(FName)
        ! Load SWF materials database
        implicit none
        character(*) :: FName
        
        call DefineUserbin(USERBIN)
        call DB_ReadSWFMaterials(trim(USERBIN)//'\'//trim(FName))
    end subroutine LoadSWFMaterialsDatabase
    
    !----------------------------------------------------------------------
    subroutine LoadETDatabase(FName)
        ! Load ET (evapotranspiration) database
        implicit none
        character(*) :: FName
        
        call DefineUserbin(USERBIN)
        call DB_ReadET(trim(USERBIN)//'\'//trim(FName))
    end subroutine LoadETDatabase
    
    !----------------------------------------------------------------------
    subroutine SetSMSParameterSet(ParameterSetNumber)
        ! Set the SMS parameter set number and display parameters
        ! Includes unit conversion if needed
        implicit none
        integer(i4), intent(in) :: ParameterSetNumber
        
        real(sp) :: LengthConversionFactor
        
        iSMSParameterSet = ParameterSetNumber
        write(TmpSTR,'(i4)') iSMSParameterSet
        call Msg('Using SMS parameter set '//trim(TmpSTR)//', '//trim(SMS_Name(iSMSParameterSet)))
        
        write(TmpSTR,'(a,'//FMT_R4//',a)')'OUTER ITERATION CONVERGENCE CRITERION  (HCLOSE)        ',SMS_HCLOSE(iSMSParameterSet),'     '//TRIM(SMS_LengthUnit(iSMSParameterSet))
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//',a)')'INNER ITERATION CONVERGENCE CRITERION  (HICLOSE)       ',SMS_HICLOSE(iSMSParameterSet),'     '//TRIM(SMS_LengthUnit(iSMSParameterSet))
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'MAXIMUM NUMBER OF OUTER ITERATIONS     (MXITER)        ',SMS_MXITER(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'MAXIMUM NUMBER OF INNER ITERATIONS     (ITER1)         ',SMS_ITER1(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'SOLVER PRINTOUT INDEX                  (IPRSMS)        ',SMS_IPRSMS(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'NONLINEAR ITERATION METHOD             (NONLINMETH)    ',SMS_NONLINMETH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'LINEAR SOLUTION METHOD                 (LINMETH)       ',SMS_LINMETH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'D-B-D WEIGHT REDUCTION FACTOR          (THETA)         ',SMS_THETA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'D-B-D WEIGHT INCREASE INCREMENT        (KAPPA)         ',SMS_KAPPA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'D-B-D PREVIOUS HISTORY FACTOR          (GAMMA)         ',SMS_GAMMA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'MOMENTUM TERM                          (AMOMENTUM)     ',SMS_AMOMENTUM(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'MAXIMUM NUMBER OF BACKTRACKS           (NUMTRACK)      ',SMS_NUMTRACK(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'BACKTRACKING TOLERANCE FACTOR          (BTOL)          ',SMS_BTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'BACKTRACKING REDUCTION FACTOR          (BREDUC)        ',SMS_BREDUC(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'BACKTRACKING RESIDUAL LIMIT            (RES_LIM)       ',SMS_RES_LIM(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'TRUNCATED NEWTON FLAG                  (ITRUNCNEWTON)  ',SMS_ITRUNCNEWTON(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,a)')      'Options                                                ',SMS_Options(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'ACCELERATION METHOD                    (IACL)          ',SMS_IACL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'EQUATION ORDERING FLAG                 (NORDER)        ',SMS_NORDER(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'LEVEL OF FILL                          (LEVEL)         ',SMS_LEVEL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'MAXIMUM NUMBER OF ORTHOGONALIZATIONS   (NORTH)         ',SMS_NORTH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'INDEX FOR USING REDUCED SYSTEM         (IREDSYS)       ',SMS_IREDSYS(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'RESIDUAL REDUCTION CONVERGE CRITERION  (RRCTOL)        ',SMS_RRCTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'INDEX FOR USING DROP TOLERANCE         (IDROPTOL)      ',SMS_IDROPTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,'//FMT_R4//')')'DROP TOLERANCE VALUE                   (EPSRN)         ',SMS_EPSRN(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        
        ! Unit conversion if needed
        LengthConversionFactor = LengthConverter(UnitsOfLength, SMS_LengthUnit(iSMSParameterSet))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg('**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')'SMS parameter set length unit:    '//trim(SMS_LengthUnit(iSMSParameterSet))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,'//FMT_R4//',a)')'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(SMS_LengthUnit(iSMSParameterSet))
            call Msg(TmpSTR)
            
            SMS_HCLOSE(iSMSParameterSet) = SMS_HCLOSE(iSMSParameterSet) * LengthConversionFactor
            SMS_HICLOSE(iSMSParameterSet) = SMS_HICLOSE(iSMSParameterSet) * LengthConversionFactor

            call Msg(' ')
            call Msg('**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iSMSParameterSet
            call Msg('Properties of SMS dataset '//trim(TmpSTR)//', '//trim(SMS_Name(iSMSParameterSet))//' after unit conversion')
            write(TmpSTR,'(a,'//FMT_R4//',a)')'OUTER ITERATION CONVERGENCE CRITERION  (HCLOSE)        ',SMS_HCLOSE(iSMSParameterSet),'     '//TRIM(UnitsOfLength)
            call Msg(trim(TmpSTR))
            write(TmpSTR,'(a,'//FMT_R4//',a)')'INNER ITERATION CONVERGENCE CRITERION  (HICLOSE)       ',SMS_HICLOSE(iSMSParameterSet),'     '//TRIM(UnitsOfLength)
            call Msg(trim(TmpSTR))
        end if
    end subroutine SetSMSParameterSet
    
    !----------------------------------------------------------------------
    subroutine ProcessDatabaseInstruction(instruction, FNumMUT, FName)
        ! Process a database-related instruction
        ! Returns the filename if instruction requires one
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        character(*), intent(out) :: FName
        
        FName = ''
        
        if(index(instruction, LocalUserbin_CMD) /= 0) then
            LocalUserbin = .true.
            read(FNumMUT,'(a)') LocalUserbinPath
            call Msg('Path to local database files: '//trim(LocalUserbinPath))
            
        else if(index(instruction, SMS_Database_CMD) /= 0) then
            read(FNumMUT,'(a)') FName
            call LoadSMSDatabase(FName)
            
        else if(index(instruction, SMSParamterSetNumber_CMD) /= 0) then
            read(FNumMUT,*) iSMSParameterSet
            call SetSMSParameterSet(iSMSParameterSet)
        
        else if(index(instruction, GWFMaterialsDatabase_CMD) /= 0) then
            read(FNumMUT,'(a)') FName
            call LoadGWFMaterialsDatabase(FName)
        
        else if(index(instruction, CLNMaterialsDatabase_CMD) /= 0) then
            read(FNumMUT,'(a)') FName
            call LoadCLNMaterialsDatabase(FName)
        
        else if(index(instruction, SWFMaterialsDatabase_CMD) /= 0) then
            read(FNumMUT,'(a)') FName
            call LoadSWFMaterialsDatabase(FName)
        
        else if(index(instruction, ET_Database_CMD) /= 0) then
            read(FNumMUT,'(a)') FName
            call LoadETDatabase(FName)
        end if
    end subroutine ProcessDatabaseInstruction

end module MUSG_Database

