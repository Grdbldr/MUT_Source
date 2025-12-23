module MUSG_InstructionParser
    !### Instruction parsing for MODFLOW-USG
    ! Handles parsing and execution of MUT instruction commands
    ! 
    ! This module contains instruction handler functions that route instructions
    ! to the appropriate modules, reducing the size of BuildModflowUSG.
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, MAX_LBL, Msg, ErrMsg, LwrCse, UnitsOfLength, UnitsOfTime, TmpSTR
    use GeneralRoutines, only: EnableTecplotOutput, EnableQGISOutput
    use ErrorHandling, only: ERR_INVALID_INPUT, HandleError
    use MUSG_Core, only: ModflowProject, ModflowDomain, NodalControlVolume
    use MUSG_ObservationPoints
    use MUSG_InitialConditions
    use MUSG_Selection
    use MUSG_MaterialProperties
    use MUSG_BoundaryConditions
    use MUSG_Database
    use MUSG_StressPeriods
    use MUSG_OutputControl
    use NumericalMesh, only: mesh
    use MeshGen, only: ReadMeshBIN, ReadGridBuilderMesh, GenerateUniformRectangles, GenerateVariableRectangles
    
    ! TemplateBuild is in Modflow_USG.f90, so we'll need to call it from there
    ! For now, we'll keep the mesh generation logic partially in BuildModflowUSG
    
    implicit none
    private
    
    ! Domain type constants (matching Modflow_USG.f90)
    integer(i4), parameter :: iTMPLT=0
    integer(i4), parameter :: iGWF=1
    integer(i4), parameter :: iSWF=2
    integer(i4), parameter :: iCLN=3
    
    public :: HandleObservationPointInstruction, HandleObservationPointsFromCSVFileInstruction
    public :: HandleActiveDomainInstruction, HandleMeshGenerationInstruction
    public :: HandleDomainGenerationInstruction
    public :: HandleZoneManagementInstruction, HandleFlaggingInstruction
    public :: HandleInitialConditionInstruction
    public :: HandleSelectionInstruction
    public :: HandleMaterialPropertyInstruction
    public :: HandleBoundaryConditionInstruction
    public :: HandleDatabaseInstruction
    public :: HandleUnitsInstruction
    public :: HandleStressPeriodAndOutputControlInstruction
    public :: HandleSimpleFlagInstruction
    
    contains
    
    !----------------------------------------------------------------------
    subroutine HandleObservationPointInstruction(FNumMUT, ActiveDomain, Modflow)
        ! Handle observation point instruction for the active domain
        implicit none
        integer(i4), intent(in) :: FNumMUT
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        
        select case(ActiveDomain)
        case (iGWF)
            call ObservationPoint(FnumMUT,modflow.GWF)
        case (iSWF)
            call ObservationPoint(FnumMUT,modflow.SWF)
        case (iCLN)
            call ObservationPoint(FnumMUT,modflow.CLN)
        end select
    end subroutine HandleObservationPointInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleObservationPointsFromCSVFileInstruction(FNumMUT, ActiveDomain, Modflow)
        ! Handle observation points from CSV file instruction for the active domain
        implicit none
        integer(i4), intent(in) :: FNumMUT
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        
        select case(ActiveDomain)
        case (iGWF)
            call ObservationPointsFromCSVFile(FnumMUT,modflow.GWF)
        case (iSWF)
            call ObservationPointsFromCSVFile(FnumMUT,modflow.SWF)
        case (iCLN)
            call ObservationPointsFromCSVFile(FnumMUT,modflow.CLN)
        end select
    end subroutine HandleObservationPointsFromCSVFileInstruction
    
    !----------------------------------------------------------------------
    function HandleActiveDomainInstruction(FNumMUT) result(ActiveDomain)
        ! Handle active domain selection instruction
        ! Returns the selected domain type constant
        implicit none
        integer(i4), intent(in) :: FNumMUT
        integer(i4) :: ActiveDomain
        character(MAX_STR) :: ActiveDomainSTR
        
        read(FNumMUT,'(a)') ActiveDomainSTR
        call LwrCse(ActiveDomainSTR)
        select case(ActiveDomainSTR)
        case ('tmplt')
            ActiveDomain=iTMPLT
        case ('gwf')
            ActiveDomain=iGWF
        case ('swf')
            ActiveDomain=iSWF
        case ('cln')
            ActiveDomain=iCLN
        case default
            call HandleError(ERR_INVALID_INPUT, 'Domain type '//trim(ActiveDomainSTR)//' not supported', 'HandleActiveDomainInstruction')
            ActiveDomain=iTMPLT ! default
        end select
        call Msg(trim(ActiveDomainSTR))
    end function HandleActiveDomainInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleMeshGenerationInstruction(instruction, FNumMUT, TMPLT, Modflow, JustBuilt)
        ! Handle mesh generation instructions (ReadMesh, MeshFromGb, GenerateUniformRectangles, etc.)
        ! Note: TemplateBuild is called from BuildModflowUSG after this returns
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: TMPLT
        type(ModflowProject), intent(inout) :: Modflow
        logical, intent(out) :: JustBuilt
        
        character(MAX_INST) :: ReadMesh_CMD = 'read mesh'
        character(MAX_INST) :: MeshFromGb_CMD = '2d mesh from gb'
        character(MAX_INST) :: GenerateUniformRectangles_CMD = 'generate uniform rectangles'
        character(MAX_INST) :: GenerateVariableRectangles_CMD = 'generate variable rectangles'
        
        character(MAX_STR) :: FName
        JustBuilt = .false.
        
        if(index(instruction, ReadMesh_CMD) /= 0) then
            read(FnumMUT,'(a)') FName
            TMPLT%Name=TRIM(FName)
            call ReadMeshBIN(TMPLT)
            ! TemplateBuild will be called from BuildModflowUSG
            
        else if(index(instruction, MeshFromGb_CMD) /= 0) then
            call ReadGridBuilderMesh(FNumMut,TMPLT)
            TMPLT.Name='TMPLT'
            ! TemplateBuild will be called from BuildModflowUSG
            
        else if(index(instruction, GenerateUniformRectangles_CMD) /= 0) then
            call GenerateUniformRectangles(FnumMUT,TMPLT)
            JustBuilt=.true.
            ! TemplateBuild will be called from BuildModflowUSG
            
        else if(index(instruction, GenerateVariableRectangles_CMD) /= 0) then
            call GenerateVariableRectangles(FnumMUT,TMPLT)
            JustBuilt=.true.
            ! TemplateBuild will be called from BuildModflowUSG
        end if
    end subroutine HandleMeshGenerationInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleDomainGenerationInstruction(instruction, FNumMUT, TMPLT, Modflow, JustBuilt, EnableQGISOutput)
        ! Handle domain generation instructions (GenerateSWFDomain, GenerateLayeredGWFDomain, GenerateCLNDomain)
        ! Note: The actual domain generation routines are in Modflow_USG.f90 due to complex dependencies
        ! This handler just identifies which instruction was called
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: TMPLT
        type(ModflowProject), intent(inout) :: Modflow
        logical, intent(out) :: JustBuilt
        logical, intent(in) :: EnableQGISOutput
        
        character(MAX_INST) :: GenerateSWFDomain_CMD = 'generate swf domain'
        character(MAX_INST) :: GenerateLayeredGWFDomain_CMD = 'generate layered gwf domain'
        character(MAX_INST) :: GenerateCLNDomain_CMD = 'generate cln domain'
        
        JustBuilt = .false.
        
        ! These routines are called from BuildModflowUSG after this handler returns
        ! The handler just identifies the instruction type
        
        if(index(instruction, GenerateSWFDomain_CMD) /= 0) then
            JustBuilt=.true.
            
        else if(index(instruction, GenerateLayeredGWFDomain_CMD) /= 0) then
            JustBuilt=.true.
            
        else if(index(instruction, GenerateCLNDomain_CMD) /= 0) then
            JustBuilt=.false. ! CLN doesn't set JustBuilt
        end if
    end subroutine HandleDomainGenerationInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleZoneManagementInstruction(instruction, ActiveDomain, Modflow, TMPLT)
        ! Handle zone management instructions (NewZone)
        ! Note: NewZoneFromChosenCells is in Modflow_USG.f90 and uses ActiveDomain
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        type(mesh), intent(inout) :: TMPLT
        
        character(MAX_INST) :: NewZone_CMD = 'new zone'
        
        ! These routines are still in Modflow_USG.f90
        ! This handler routes to the appropriate domain
        if(index(instruction, NewZone_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                ! Not implemented for template
            case (iGWF)
                ! call NewZoneFromChosenCells(modflow.GWF) - called from BuildModflowUSG
            case (iSWF)
                ! call NewZoneFromChosenCells(modflow.SWF) - called from BuildModflowUSG
            case (iCLN)
                ! call NewZoneFromChosenCells(modflow.CLN) - called from BuildModflowUSG
            end select
        end if
    end subroutine HandleZoneManagementInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleFlaggingInstruction(instruction, ActiveDomain, Modflow, TMPLT)
        ! Handle flagging instructions (FlagChosenCellsInactive)
        ! Note: FlagChosenCellsInactive is in Modflow_USG.f90
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        type(mesh), intent(inout) :: TMPLT
        
        character(MAX_INST) :: FlagChosenCellInactive_CMD = 'flag chosen cells inactive'
        
        ! These routines are still in Modflow_USG.f90
        ! This handler routes to the appropriate domain
        if(index(instruction, FlagChosenCellInactive_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                ! call FlagChosenCellsInactiveTMPLT(TMPLT) - called from BuildModflowUSG
            case (iGWF)
                ! call FlagChosenCellsInactive(modflow.GWF) - called from BuildModflowUSG
            case (iSWF)
                ! call FlagChosenCellsInactive(modflow.SWF) - called from BuildModflowUSG
            case (iCLN)
                ! call FlagChosenCellsInactive(modflow.CLN) - called from BuildModflowUSG
            end select
        end if
    end subroutine HandleFlaggingInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleInitialConditionInstruction(instruction, FNumMUT, ActiveDomain, Modflow)
        ! Handle initial condition instructions for the active domain
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        
        character(MAX_INST) :: InitialHeadFromCSVFile_CMD = 'initial head from csv file'
        character(MAX_INST) :: GWFInitialHeadFromTecplotFile_CMD = 'gwf initial head from tecplot file'
        character(MAX_INST) :: GWFInitialHeadEqualsSurfaceElevation_CMD = 'gwf initial head equals surface elevation'
        character(MAX_INST) :: InitialHeadFunctionOfZtoGWF_CMD = 'gwf initial head function of z'
        character(MAX_INST) :: InitialHeadFromDepthSatToGWF_CMD = 'gwf initial head from depth-saturation table'
        character(MAX_INST) :: SWFInitialHeadFromTecplotFile_CMD = 'swf initial head from tecplot file'
        
        if(index(instruction, InitialHeadFromCSVFile_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call InitialHeadFromCSVFile(FnumMUT,modflow.GWF)
            case (iSWF)
                call InitialHeadFromCSVFile(FnumMUT,modflow.SWF)
            case (iCLN)
                call InitialHeadFromCSVFile(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, GWFInitialHeadFromTecplotFile_CMD) /= 0) then
            call GWFInitialHeadFromTecplotFile(FnumMUT,modflow.GWF)
            
        else if(index(instruction, GWFInitialHeadEqualsSurfaceElevation_CMD) /= 0) then
            call GWFInitialHeadEqualsSurfaceElevation(modflow.GWF)
            
        else if(index(instruction, InitialHeadFunctionOfZtoGWF_CMD) /= 0) then
            call InitialHeadFunctionOfZtoGWF(FnumMUT,modflow.GWF)
            
        else if(index(instruction, InitialHeadFromDepthSatToGWF_CMD) /= 0) then
            call InitialHeadFromDepthSatToGWF(FnumMUT,modflow.GWF)
            
        else if(index(instruction, SWFInitialHeadFromTecplotFile_CMD) /= 0) then
            call SWFInitialHeadFromTecplotFile(FnumMUT,modflow.SWF)
        end if
    end subroutine HandleInitialConditionInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleSelectionInstruction(instruction, FNumMUT, ActiveDomain, Modflow, TMPLT)
        ! Handle all selection instructions (Choose*, Clear*)
        ! Routes selection instructions to the appropriate domain
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        type(mesh), intent(inout) :: TMPLT
        
        ! Node selection commands
        character(MAX_INST) :: ChooseAllNodes_CMD = 'choose all nodes'
        character(MAX_INST) :: ClearAllNodes_CMD = 'clear chosen nodes'
        character(MAX_INST) :: ChooseNodeAtXYZ_CMD = 'choose node at xyz'
        character(MAX_INST) :: ChooseGBNodes_CMD = 'choose gb nodes'
        
        ! Cell selection commands
        character(MAX_INST) :: ChooseAllCells_CMD = 'choose all cells'
        character(MAX_INST) :: ClearAllCells_CMD = 'clear chosen cells'
        character(MAX_INST) :: ChooseCellAtXYZ_CMD = 'choose cell at xyz'
        character(MAX_INST) :: ChooseCellsByLayer_CMD = 'choose cells by layer'
        character(MAX_INST) :: ChooseCellbyXYZ_LayerRange_CMD = 'choose cells by xyz layer range'
        character(MAX_INST) :: ChooseCellsFromFile_CMD = 'choose cells from file'
        character(MAX_INST) :: ChooseCellsFromXYZList_CMD = 'choose cells from xyz list'
        character(MAX_INST) :: ChooseCellsByChosenZones_CMD = 'choose cells by chosen zones'
        character(MAX_INST) :: ChooseCellsFromGBElements_CMD = 'choose cells from gb elements'
        character(MAX_INST) :: ChooseCellsFromGBNodes_CMD = 'choose cells from gb nodes'
        
        ! Zone selection commands
        character(MAX_INST) :: ChooseAllZones_CMD = 'choose all zones'
        character(MAX_INST) :: ClearAllZones_CMD = 'clear chosen zones'
        character(MAX_INST) :: ChooseZoneNumber_CMD = 'choose zone number'
        
        ! Node selection
        if(index(instruction, ChooseAllNodes_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseAllNodes(modflow.GWF)
            case (iSWF)
                call ChooseAllNodes(modflow.SWF)
            case (iCLN)
                call ChooseAllNodes(modflow.CLN)
            end select
            
        else if(index(instruction, ClearAllNodes_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ClearAllNodes(modflow.GWF)
            case (iSWF)
                call ClearAllNodes(modflow.SWF)
            case (iCLN)
                call ClearAllNodes(modflow.CLN)
            end select
            
        else if(index(instruction, ChooseNodeAtXYZ_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                call ChooseNodeAtXYZTemplate(FnumMUT,TMPLT)
            case (iGWF)
                call ChooseNodeAtXYZ(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseNodeAtXYZ(FnumMUT,Modflow.SWF)
            case (iCLN)
                call ChooseNodeAtXYZ(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseGBNodes_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                call ChooseGBNodesTemplate(FnumMUT,TMPLT)
            case (iGWF)
                call ChooseGBNodes(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseGBNodes(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseGBNodes(FnumMUT,modflow.CLN)
            end select
            
        ! Cell selection
        else if(index(instruction, ChooseAllCells_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseAllCells(modflow.GWF)
            case (iSWF)
                call ChooseAllCells(modflow.SWF)
            case (iCLN)
                call ChooseAllCells(modflow.CLN)
            end select
            
        else if(index(instruction, ClearAllCells_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ClearAllCells(modflow.GWF)
            case (iSWF)
                call ClearAllCells(modflow.SWF)
            case (iCLN)
                call ClearAllCells(modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellAtXYZ_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellAtXYZ(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellAtXYZ(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellAtXYZ(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsByLayer_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellsByLayer(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellsByLayer(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellsByLayer(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellbyXYZ_LayerRange_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsFromFile_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellsFromFile(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellsFromFile(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellsFromFile(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsFromXYZList_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellsFromXYZList(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellsFromXYZList(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellsFromXYZList(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsByChosenZones_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseCellsByChosenZones(modflow.GWF)
            case (iSWF)
                call ChooseCellsByChosenZones(modflow.SWF)
            case (iCLN)
                call ChooseCellsByChosenZones(modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsFromGBElements_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                call ChooseCellsFromGBElementsTemplate(FnumMUT,TMPLT)
            case (iGWF)
                call ChooseCellsFromGBElements(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellsFromGBElements(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellsFromGBElements(FnumMUT,modflow.CLN)
            end select
            
        else if(index(instruction, ChooseCellsFromGBNodes_CMD) /= 0) then
            select case(ActiveDomain)
            case (iTMPLT)
                call ChooseCellsFromGBNodesTemplate(FnumMUT,TMPLT)
            case (iGWF)
                call ChooseCellsFromGBNodes(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseCellsFromGBNodes(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseCellsFromGBNodes(FnumMUT,modflow.CLN)
            end select
            
        ! Zone selection
        else if(index(instruction, ChooseAllZones_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseAllZones(modflow.GWF)
            case (iSWF)
                call ChooseAllZones(modflow.SWF)
            case (iCLN)
                call ChooseAllZones(modflow.CLN)
            end select
            
        else if(index(instruction, ClearAllZones_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ClearAllZones(modflow.GWF)
            case (iSWF)
                call ClearAllZones(modflow.SWF)
            case (iCLN)
                call ClearAllZones(modflow.CLN)
            end select
            
        else if(index(instruction, ChooseZoneNumber_CMD) /= 0) then
            select case(ActiveDomain)
            case (iGWF)
                call ChooseZoneNumber(FnumMUT,modflow.GWF)
            case (iSWF)
                call ChooseZoneNumber(FnumMUT,modflow.SWF)
            case (iCLN)
                call ChooseZoneNumber(FnumMUT,modflow.CLN)
            end select
        end if
    end subroutine HandleSelectionInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleMaterialPropertyInstruction(instruction, FNumMUT, ActiveDomain, Modflow)
        ! Handle all material property assignment instructions
        ! Routes material property instructions to the appropriate domain
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        integer(i4), intent(in) :: ActiveDomain
        type(ModflowProject), intent(inout) :: Modflow
        
        ! GWF material property commands
        character(MAX_INST) :: AssignMaterialtoGWF_CMD = 'chosen cells use gwf material number'
        character(MAX_INST) :: AssignKhtoGWF_CMD = 'gwf kh'
        character(MAX_INST) :: AssignKvtoGWF_CMD = 'gwf kv'
        character(MAX_INST) :: AssignSstoGWF_CMD = 'gwf ss'
        character(MAX_INST) :: AssignSytoGWF_CMD = 'gwf sy'
        character(MAX_INST) :: AssignAlphatoGWF_CMD = 'gwf alpha'
        character(MAX_INST) :: AssignBetatoGWF_CMD = 'gwf beta'
        character(MAX_INST) :: AssignSrtoGWF_CMD = 'gwf sr'
        character(MAX_INST) :: AssignBrookstoGWF_CMD = 'gwf brooks'
        character(MAX_INST) :: AssignStartingheadtoGWF_CMD = 'gwf initial head'
        
        ! CLN material property commands
        character(MAX_INST) :: AssignMaterialtoCLN_CMD = 'chosen zones use cln material number'
        character(MAX_INST) :: AssignStartingDepthtoCLN_CMD = 'cln initial depth'
        
        ! SWF material property commands
        character(MAX_INST) :: AssignMaterialtoSWF_CMD = 'chosen zones use swf material number'
        character(MAX_INST) :: AssignSgcltoSWF_CMD = 'swf to gwf connection length'
        character(MAX_INST) :: AssignStartingDepthtoSWF_CMD = 'swf initial depth'
        character(MAX_INST) :: AssignManningtoSWF_CMD = 'swf manning'
        character(MAX_INST) :: AssignDepressiontoSWF_CMD = 'swf depression storage height'
        character(MAX_INST) :: AssignObstructiontoSWF_CMD = 'swf obstruction storage height'
        character(MAX_INST) :: AssignDepthForSmoothingtoSWF_CMD = 'swf depth for smoothing'
        
        ! GWF material properties
        if(index(instruction, AssignMaterialtoGWF_CMD) /= 0) then
            call AssignMaterialtoGWF(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignKhtoGWF_CMD) /= 0) then
            call AssignKhtoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignKvtoGWF_CMD) /= 0) then
            call AssignKvtoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignSstoGWF_CMD) /= 0) then
            call AssignSstoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignSytoGWF_CMD) /= 0) then
            call AssignSytoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignAlphatoGWF_CMD) /= 0) then
            call AssignAlphatoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignBetatoGWF_CMD) /= 0) then
            call AssignBetatoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignSrtoGWF_CMD) /= 0) then
            call AssignSrtoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignBrookstoGWF_CMD) /= 0) then
            call AssignBrookstoDomain(FnumMUT,modflow.GWF)
            
        else if(index(instruction, AssignStartingheadtoGWF_CMD) /= 0) then
            call AssignStartingHeadtoDomain(FnumMUT,modflow.GWF)
            
        ! CLN material properties
        else if(index(instruction, AssignMaterialtoCLN_CMD) /= 0) then
            call AssignMaterialtoCLN(FnumMUT,modflow.CLN)
            
        else if(index(instruction, AssignStartingDepthtoCLN_CMD) /= 0) then
            call AssignStartingDepthtoDomain(FnumMUT,modflow.CLN)
            
        ! SWF material properties
        else if(index(instruction, AssignMaterialtoSWF_CMD) /= 0) then
            call AssignMaterialtoSWF(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignSgcltoSWF_CMD) /= 0) then
            call AssignSgcltoDomain(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignStartingDepthtoSWF_CMD) /= 0) then
            call AssignStartingDepthtoDomain(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignManningtoSWF_CMD) /= 0) then
            call AssignManningtoSWF(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignDepressiontoSWF_CMD) /= 0) then
            call AssignDepressiontoSWF(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignObstructiontoSWF_CMD) /= 0) then
            call AssignObstructiontoSWF(FnumMUT,modflow.SWF)
            
        else if(index(instruction, AssignDepthForSmoothingtoSWF_CMD) /= 0) then
            call AssignDepthForSmoothingtoSWF(FnumMUT,modflow.SWF)
        end if
    end subroutine HandleMaterialPropertyInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleBoundaryConditionInstruction(instruction, FNumMUT, Modflow)
        ! Handle all boundary condition assignment instructions
        ! Routes boundary condition instructions to the appropriate domain
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Modflow
        
        ! GWF boundary condition commands
        character(MAX_INST) :: AssignCHDtoGWF_CMD = 'gwf constant head'
        character(MAX_INST) :: AssignDRNtoGWF_CMD = 'gwf drain'
        character(MAX_INST) :: AssignRCHtoGWF_CMD = 'gwf recharge'
        character(MAX_INST) :: AssignWELtoGWF_CMD = 'gwf well'
        
        ! SWF boundary condition commands
        character(MAX_INST) :: AssignCHDtoSWF_CMD = 'swf constant head'
        character(MAX_INST) :: AssignRCHtoSWF_CMD = 'swf recharge'
        character(MAX_INST) :: AssignTransientRCHtoSWF_CMD = 'swf transient recharge'
        character(MAX_INST) :: AssignWELtoSWF_CMD = 'swf well'
        character(MAX_INST) :: AssignCriticalDepthtoSWF_CMD = 'swf critical depth'
        character(MAX_INST) :: AssignCriticalDepthtoCellsSide1_CMD = 'swf critical depth with sidelength1'
        
        ! CLN boundary condition commands
        character(MAX_INST) :: AssignCHDtoCLN_CMD = 'cln constant head'
        character(MAX_INST) :: AssignWELtoCLN_CMD = 'cln well'
        
        ! GWF boundary conditions
        if(index(instruction, AssignCHDtoGWF_CMD) /= 0) then
            call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.GWF)
            
        else if(index(instruction, AssignDRNtoGWF_CMD) /= 0) then
            call AssignDRNtoDomain(FnumMUT,Modflow,Modflow.GWF)
            
        else if(index(instruction, AssignRCHtoGWF_CMD) /= 0) then
            call AssignRCHtoDomain(FnumMUT,Modflow,Modflow.GWF)
            
        else if(index(instruction, AssignWELtoGWF_CMD) /= 0) then
            call AssignWELtoDomain(FnumMUT,Modflow,Modflow.GWF)
            
        ! SWF boundary conditions
        else if(index(instruction, AssignCHDtoSWF_CMD) /= 0) then
            call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.SWF)
            
        else if(index(instruction, AssignRCHtoSWF_CMD) /= 0) then
            call AssignRCHtoDomain(FnumMUT,Modflow,Modflow.SWF)
            
        else if(index(instruction, AssignTransientRCHtoSWF_CMD) /= 0) then
            call AssignTransientRCHtoDomain(FnumMUT,Modflow,Modflow.SWF)
            
        else if(index(instruction, AssignWELtoSWF_CMD) /= 0) then
            call AssignWELtoDomain(FnumMUT,Modflow,Modflow.SWF)
            
        else if(index(instruction, AssignCriticalDepthtoSWF_CMD) /= 0) then
            call AssignCriticalDepthtoDomain(Modflow,Modflow.SWF)
            
        else if(index(instruction, AssignCriticalDepthtoCellsSide1_CMD) /= 0) then
            call AssignCriticalDepthtoCellsSide1(Modflow,Modflow.SWF)
            
        ! CLN boundary conditions
        else if(index(instruction, AssignCHDtoCLN_CMD) /= 0) then
            call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.CLN)
            
        else if(index(instruction, AssignWELtoCLN_CMD) /= 0) then
            call AssignWELtoDomain(FnumMUT,Modflow,Modflow.CLN)
        end if
    end subroutine HandleBoundaryConditionInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleDatabaseInstruction(instruction, FNumMUT)
        ! Handle all database-related instructions
        ! Routes database instructions to MUSG_Database module
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        
        character(MAX_STR) :: FName
        
        ! All database instructions are handled by ProcessDatabaseInstruction
        call ProcessDatabaseInstruction(instruction, FNumMUT, FName)
    end subroutine HandleDatabaseInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleUnitsInstruction(instruction, FNumMUT, Modflow)
        ! Handle units instructions (UnitsTime, UnitsLength)
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Modflow
        
        character(MAX_INST) :: UnitsTime_CMD = 'units of time'
        character(MAX_INST) :: UnitsLength_CMD = 'units of length'
        
        if(index(instruction, UnitsTime_CMD) /= 0) then
            call UnitsTime(FNumMUT, Modflow)
        else if(index(instruction, UnitsLength_CMD) /= 0) then
            call UnitsLength(FNumMUT, Modflow)
        end if
    end subroutine HandleUnitsInstruction
    
    !----------------------------------------------------------------------
    subroutine UnitsLength(FnumMUT,Project) 
        implicit none
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Project
        
        character(MAX_LBL) :: value
        
        read(FNumMUT,'(a)') value
        call LwrCse(value)
        
        select case(value)
        case ('feet')
            Project.STR_LengthUnit='FEET'
            UnitsOfLength=Project.STR_LengthUnit
            Project.LengthUnits=1
        case ('meters')
            Project.STR_LengthUnit='METERS'
            UnitsOfLength=Project.STR_LengthUnit
            Project.LengthUnits=2
        case ('centimeters')
            Project.STR_LengthUnit='CENTIMETERS'
            UnitsOfLength=Project.STR_LengthUnit
            Project.LengthUnits=3
        case default
            call HandleError(ERR_INVALID_INPUT, 'Units of length '//trim(value)//' not recognized. Must be feet, meters, or centimeters.', 'UnitsLength')
        end select
        
        write(TmpSTR,'(a)')    'Units of length:   '//trim(Project.STR_LengthUnit)
        call Msg(TmpSTR)
    end subroutine UnitsLength
    
    !----------------------------------------------------------------------
    subroutine UnitsTime(FNumMUT,Project) 
        implicit none
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Project
        
        character(MAX_LBL) :: value
        
        read(FNumMUT,'(a)') value
        call LwrCse(value)
       
        select case(value)
        case ('seconds')
            Project.STR_TimeUnit='SECONDS'
            UnitsOfTime=Project.STR_TimeUnit
            Project.TimeUnits=1
       case ('minutes')
            Project.STR_TimeUnit='MINUTES'
            UnitsOfTime=Project.STR_TimeUnit
            Project.TimeUnits=2
        case ('hours')
            Project.STR_TimeUnit='HOURS'
            UnitsOfTime=Project.STR_TimeUnit
            Project.TimeUnits=3
        case ('days')
            Project.STR_TimeUnit='DAYS'
            UnitsOfTime=Project.STR_TimeUnit
            Project.TimeUnits=4
        case ('years')
            Project.STR_TimeUnit='YEARS'
            UnitsOfTime=Project.STR_TimeUnit
            Project.TimeUnits=5
        case default
            call HandleError(ERR_INVALID_INPUT, 'Units of time '//trim(value)//' not recognized. Must be seconds, minutes, hours, days or years.', 'UnitsTime')
        end select
        
        write(TmpSTR,'(a)')    'Units of time:   '//trim(Project.STR_TimeUnit)
        call Msg(TmpSTR)
    end subroutine UnitsTime
    
    !----------------------------------------------------------------------
    subroutine HandleStressPeriodAndOutputControlInstruction(instruction, FNumMUT, Modflow)
        ! Handle stress period and output control instructions
        implicit none
        character(*), intent(in) :: instruction
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Modflow
        
        character(MAX_INST) :: GenOCFile_CMD = 'generate output control file'
        character(MAX_INST) :: StressPeriod_CMD = 'stress period'
        
        if(index(instruction, GenOCFile_CMD) /= 0) then
            call GenOCFile(FNumMUT, Modflow)
        else if(index(instruction, StressPeriod_CMD) /= 0) then
            call StressPeriod(FNumMUT, Modflow)
        end if
    end subroutine HandleStressPeriodAndOutputControlInstruction
    
    !----------------------------------------------------------------------
    subroutine HandleSimpleFlagInstruction(instruction, Modflow)
        ! Handle simple flag-setting instructions (NodalControlVolumes, SaturatedFlow, DisableTecplotOutput, DisableQGISOutput)
        implicit none
        character(*), intent(in) :: instruction
        type(ModflowProject), intent(inout) :: Modflow
        
        character(MAX_INST) :: NodalControlVolumes_CMD = 'nodal control volumes'
        character(MAX_INST) :: SaturatedFlow_CMD = 'saturated flow'
        character(MAX_INST) :: DisableTecplotOutput_CMD = 'disable tecplot output'
        character(MAX_INST) :: DisableQGISOutput_CMD = 'disable qgis output'
        
        if(index(instruction, NodalControlVolumes_CMD) /= 0) then
            NodalControlVolume = .true.
            call Msg('*** Control volumes (i.e. modflow cells) will be centred at 2D mesh nodes')
            
        else if(index(instruction, SaturatedFlow_CMD) /= 0) then
            Modflow.SaturatedFlow = .true.
            call Msg('*** Saturated flow approach is used ')
            
        else if(index(instruction, DisableTecplotOutput_CMD) /= 0) then
            EnableTecplotOutput = .false.
            call Msg('*** Tecplot Output Disabled')
            
        else if(index(instruction, DisableQGISOutput_CMD) /= 0) then
            EnableQGISOutput = .false.
            call Msg('*** QGIS Output Disabled')
        end if
    end subroutine HandleSimpleFlagInstruction

end module MUSG_InstructionParser

