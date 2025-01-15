module MUSG !
    use GeneralRoutines
    !use ProcessCSV
    use fem
    use materials
    use MeshGeneration
    use tecplot
    use global
    use CLN1MODULE
    use SWF1MODULE
    use hgs
    implicit none
    
    ! Pre/Post-processor for Modflow project files Fall 2023
    ! Added instructions
    character(MAX_INST) :: instruction
    
    ! Pre-processing i.e. building a modflow structure
    ! By default, we will assume a node-centred control volume
    
    ! --------------------------------------------------Mesh definition section
    ! By default, the 2D template mesh is converted to Modflow mesh using a mesh-centred control volume approach
    ! This option changes it to a node-centred control volume approach
    character(MAX_INST) :: NodalControlVolumes_CMD='nodal control volumes'

    ! Units
    character(MAX_INST) :: UnitsTime_CMD	        =   'units of time'
    character(MAX_INST) :: UnitsLength_CMD	        =   'units of length'
    
    !---------------------------------------------------Database
    character(MAX_INST) :: SMS_Database_CMD	        =   'sms database'
    character(MAX_INST) :: GWFMaterialsDatabase_CMD	=   'gwf materials database'
    character(MAX_INST) :: CLNMaterialsDatabase_CMD	=   'cln materials database'
    character(MAX_INST) :: SWFMaterialsDatabase_CMD	=   'swf materials database'
    character(MAX_INST) :: ET_Database_CMD	        =   'et database'
    

    ! Ways to define the 2D template mesh
    character(MAX_INST) :: MeshFromGb_CMD          =   '2d mesh from gb'
    character(MAX_INST) :: QuadtreeMeshFromGWV_CMD =   '2d quadtree mesh from groundwater vistas'
    character(MAX_INST) :: GenerateUniformRectangles_CMD  =   'generate uniform rectangles'
    ! There are many other possible 2d mesh definition options e.g.
    !character(MAX_INST), parameter :: gv_rects            =   'generate variable rectangles'
    !character(MAX_INST), parameter :: g_rects_i           =   'generate rectangles interactive' 
    
    character(MAX_INST) :: GenerateSWFDomain_CMD		=   'generate swf domain'

    ! Generate a CLN network
    character(MAX_INST) :: GenerateCLNDomain_CMD		=   'generate cln domain'
    
    ! Generate a layered 3D modflow mesh from a 2D mesh
    character(MAX_INST) :: GenerateLayeredGWFDomain_CMD		=   'generate layered gwf domain'
        
    !---------------------------------------------------Selection and assignment options
    character(MAX_INST) :: ActiveDomain_CMD	                    =   'active domain'

    character(MAX_INST) :: ChooseAllCells_CMD  	                =   'choose all cells'
    character(MAX_INST) :: ClearAllCells_CMD	                =   'clear chosen cells'
    character(MAX_INST) :: ChooseCellsByLayer_CMD  	            =   'choose cells by layer'
    character(MAX_INST) :: ChooseCellAtXYZ_CMD                  =   'choose cell at xyz'
    character(MAX_INST) :: ChooseCellbyXYZ_LayerRange_CMD       =   'choose cells by xyz layer range'
    character(MAX_INST) :: ChooseCellsFromGBElements_CMD        =   'choose cells from gb elements'
    character(MAX_INST) :: ChooseCellsFromGBNodes_CMD	        =   'choose cells from gb nodes'
    character(MAX_INST) :: ChooseCellsFromFile_CMD              =   'choose cells from file'
    character(MAX_INST) :: ChooseCellsByChosenZones_CMD         =   'choose cells by chosen zones'

    character(MAX_INST) :: NewZone_CMD  	                    =   'new zone'
    character(MAX_INST) :: ChooseAllZones_CMD  	                =   'choose all zones'
    character(MAX_INST) :: ClearAllZones_CMD	                =   'clear chosen zones'
    character(MAX_INST) :: ChooseZoneNumber_CMD	                =   'choose zone number'

    character(MAX_INST) :: ClearAllNodes_CMD	                =   'clear chosen nodes'
    character(MAX_INST) :: ChooseAllNodes_CMD                   =   'choose all nodes'
    character(MAX_INST) :: ChooseNodeAtXYZ_CMD                  =   'choose node at xyz'
    character(MAX_INST) :: ChooseGBNodes_CMD	                =   'choose gb nodes'

    character(MAX_INST) :: FlagChosenCellInactive_CMD           =   'flag chosen cells inactive'
    character(MAX_INST) :: FlagChosenNodesAsOuterBoundary_CMD   =   'flag chosen nodes as outer boundary'
    

    !---------------------------------------------------Initial conditions
    character(MAX_INST) :: AssignStartingHeadtoGWF_CMD	    =   'gwf initial head'
    character(MAX_INST) :: InitialHeadFunctionOfZtoGWF_CMD  =   'gwf initial head function of z' 
    character(MAX_INST) :: AssignStartingDepthtoSWF_CMD	    =   'swf initial depth'
    character(MAX_INST) :: AssignStartingDepthtoCLN_CMD	    =   'cln initial depth'
    
    !---------------------------------------------------Boundary conditions
    character(MAX_INST) :: AssignCHDtoGWF_CMD		    =   'gwf constant head'
    character(MAX_INST) :: AssignDRNtoGWF_CMD		    =   'gwf drain'
    character(MAX_INST) :: AssignRCHtoGWF_CMD		    =   'gwf recharge'
    character(MAX_INST) :: AssignWELtoGWF_CMD		    =   'gwf well'
    character(MAX_INST) :: AssignCHDtoSWF_CMD		    =   'swf constant head'
    character(MAX_INST) :: AssignRCHtoSWF_CMD		    =   'swf recharge'
    character(MAX_INST) :: AssignWELtoSWF_CMD		    =   'swf well'
    character(MAX_INST) :: AssignCriticalDepthtoSWF_CMD	        =   'swf critical depth'
    character(MAX_INST) :: AssignCriticalDepthtoCellsSide1_CMD	=   'swf critical depth with sidelength1'
    character(MAX_INST) :: AssignCHDtoCLN_CMD           =   'cln constant head'
    character(MAX_INST) :: AssignWELtoCLN_CMD		    =   'cln well'
    
    !---------------------------------------------------GWF Properties
    character(MAX_INST) :: AssignMaterialtoGWF_CMD		=   'chosen cells use gwf material number'
    character(MAX_INST) :: AssignKhtoGWF_CMD		        =   'gwf kh'
    character(MAX_INST) :: AssignKvtoGWF_CMD		        =   'gwf kv'
    character(MAX_INST) :: AssignSstoGWF_CMD		        =   'gwf ss'
    character(MAX_INST) :: AssignSytoGWF_CMD		        =   'gwf sy'
    character(MAX_INST) :: AssignAlphatoGWF_CMD	    	    =   'gwf alpha'
    character(MAX_INST) :: AssignBetatoGWF_CMD	    	    =   'gwf beta'
    character(MAX_INST) :: AssignSrtoGWF_CMD		        =   'gwf sr'
    character(MAX_INST) :: AssignBrookstoGWF_CMD		    =   'gwf brooks'
        
    !---------------------------------------------------CLN Properties
    character(MAX_INST) :: AssignMaterialtoCLN_CMD		    =   'chosen zones use cln material number'
    !character(MAX_INST) :: AssignSizetoCLN_CMD		        =   'cln radius or width'

    !---------------------------------------------------SWF Properties
    character(MAX_INST) :: AssignMaterialtoSWF_CMD		    =   'chosen zones use swf material number'
    character(MAX_INST) :: AssignSgcltoSWF_CMD		        =   'swf to gwf connection length'
    character(MAX_INST) :: AssignManningtoSWF_CMD		    =   'swf manning'
    character(MAX_INST) :: AssignDepressiontoSWF_CMD		=   'swf depression storage height'
    character(MAX_INST) :: AssignObstructiontoSWF_CMD		=   'swf obstruction storage height'
    character(MAX_INST) :: AssignDepthForSmoothingtoSWF_CMD	=   'swf depth for smoothing'
        
    !---------------------------------------------------SMS Dataset
    character(MAX_INST) :: SMSParamterSetNumber_CMD		    =   'sms parameter set number'


    ! --------------------------------------------------Modflow project definition section
    character(MAX_INST) :: ModflowPrefix_CMD='modflow prefix'
    character(MAX_INST) :: GenOCFile_CMD='generate output control file'

    character(MAX_INST) :: StressPeriod_CMD='stress period'

    character(MAX_INST) :: HGSToModflowStructure_CMD='hgs to modflow structure'


    ! Post-processing modflow output files
    character(MAX_INST) :: ModflowOutputToModflowStructure_CMD='modflow output to modflow structure'
    
    integer  :: FNum
    character(MAX_STR) :: line

    logical :: JustBuilt=.false.
    
    integer :: ActiveDomain=0
    character(5) :: ActiveDomainSTR
    integer, parameter :: iTMPLT=0
    integer, parameter :: iGWF=1
    integer, parameter :: iSWF=2
    integer, parameter :: iCLN=3

    
    ! Added for Modflow-USG Tools
    

    type ModflowDomain
        ! common to all types of domains: GWF, CLN, SWF, ...
        logical :: IsDefined=.false.      ! this type of domain has been defined 
        character(128) :: MeshType      ! structured or unstructured?
        character(128) :: ElementType      ! for tecplot, febrick (GWF), fequadrilateral(SWF), felineseg(CLN)

        character(10) :: Name='none'
        integer :: nCells                ! number of cells in the mesh
        integer :: nLayers                 ! number of layers in the mesh 
        integer :: nNodes               ! number of nodes in the mesh for Tecplot visualization 
        integer :: nElements               ! number of nodes in the mesh for Tecplot visualization 
        !
        integer :: nNodesPerCell        ! number of nodes/cell  
        integer, allocatable :: iNode(:,:)  ! node list for cell (nCells, nNodesPerElement)
        
        !integer :: nEdgesPerCell        ! number of edges/cell  (for determining neighbours)
        !integer, allocatable :: xEdge(:,:)  ! x-coord of centroid for cell edge(nCells, nNedgesPerCell)
        !integer, allocatable :: yEdge(:,:)  ! y-coord of centroid for cell edge(nCells, nNedgesPerCell)
        !integer, allocatable :: zEdge(:,:)  ! z-coord of centroid for cell edge(nCells, nNedgesPerCell)

        integer :: iz      ! is 1 if the elevations of node and mesh elements vertices are supplied; 0 otherwise
        integer :: ic      ! is 1 if the cell specifications associated with each node are supplied; 0 otherwise

           
        ! arrays of size nCells
        real(dr), allocatable :: xCell(:)      ! cell x coordinate
        real(dr), allocatable :: yCell(:)      ! cell y coordinate
        real(dr), allocatable :: zCell(:)      ! cell z coordinate
        real(dr), allocatable :: Top(:)        ! cell top elevation
        real(dr), allocatable :: Bottom(:)     ! cell bottom elevation
        integer, allocatable :: iLayer(:)      ! cell layer number
        integer, allocatable :: iZone(:)       ! cell zone number
        
        ! inner circles
        real(dr), allocatable :: CellArea(:)        ! projected area of cell in XY
        real(dr), allocatable :: rCircle(:)         ! projected inner circle radius from TMPLT(e.g. GridBldr)
        real(dr), allocatable :: SideLength(:,:)    ! projected side length from TMPLT(e.g. GridBldr), nNodesPerElement by nCells
        real(dr), allocatable :: xSide(:,:)         ! projected x coordinate of inner circle radius tangent to side
        real(dr), allocatable :: ySide(:,:)         ! projected y coordinate of inner circle radius tangent to side
        
        real(dr), allocatable :: ConnectionLength(:)    ! CLN in modflow
        real(dr), allocatable :: PerpendicularArea(:)   ! FAHL in modflow
        
        integer :: NCLNGWC      ! # of CLN to GWF connections
        integer :: NCONDUITYP   ! number of circular CLN's
        integer :: NRECTYP      ! number of rectangular CLN's
        
        
        

        !! Tecplot face neighbours
        !integer, allocatable :: cell(:)
        !integer, allocatable :: face(:)
        !integer, allocatable :: neighbour(:)
        

        
        real(dr), allocatable :: StartingHeads(:)   ! STRT in modflow i.e. initial heads
        integer :: nCHDCells=0        
        real(dr), allocatable :: ConstantHead(:)  ! CHD assigned head value

        real(dr), allocatable :: Recharge(:)  ! RCH assigned recharge value
        integer :: nRCHoption  ! RCH option (nrchop in Modflow)
        
        integer :: nDRNCells=0        
        real(dr), allocatable :: DrainElevation(:)  ! DRN assigned Drain Elevation value
        real(dr), allocatable :: DrainConductance(:)  ! DRN assigned Drain Conductance value

        integer :: nWELCells=0        
        real(dr), allocatable :: PumpingRate(:)  ! WEL assigned Pumping Rate value
        
        real(dr), allocatable :: CriticalDepthLength(:)  ! SWBC assigned critical depth boundary cell length value
        integer :: nSWBCCells=0        
        
        ! ia ja arrays for cell connections
        integer, allocatable :: njag      ! total number of connections for mesh
        integer, allocatable :: ia(:)      ! size nCells, number of connections/cell
        integer, allocatable :: ja(:)      ! size total number of connections for mesh, cell connection lists

        ! of size nNodes
        real(dr), allocatable :: x(:) 
        real(dr), allocatable :: y(:)
        real(dr), allocatable :: z(:)
        
        integer, allocatable :: ibound(:)
        integer, allocatable :: laybcd(:)  ! size nLayers, non-zero value indicates layer has a quasi-3D confining bed below
        integer :: nodelay    ! for now assume a constant for stacked mesh with no vertical refinement
        
        integer, allocatable :: LayTyp(:)  ! size nLayers, layer type
        integer, allocatable :: LayAvg(:)  ! size nLayers, layer type
        real(dr), allocatable :: chani(:)  ! size nLayers, layer type
        integer, allocatable :: layvka(:)  ! size nLayers, layer type
        integer, allocatable :: laywet(:)  ! size nLayers, layer type
        
        integer :: nZones                  ! number of zones in domain
        integer,allocatable	:: Cell_is(:)  ! size ncells,  bit setting e.g. chosen/not chosen
        integer,allocatable	:: Node_is(:)  ! size nNodes,  bit setting e.g. chosen/not chosen
        integer,allocatable	:: Zone_is(:)  ! size nZones,  bit setting e.g. chosen/not chosen

    
        
        
        real, allocatable :: hnew(:)  ! initial head
        
        ! .HDS file
        character(128) :: FNameHDS
        integer :: iHDS
        real, allocatable :: Head(:,:)
        
        ! .DDN file
        character(128) :: FNameDDN
        integer :: iDDN
	    real, allocatable :: Drawdown(:,:)
        
        ! .CBB file 
        integer :: nComp
        character(128) :: FNameCBB
        integer :: iCBB
        real, allocatable :: Cbb_STORAGE(:,:)
	    real, allocatable :: Cbb_CONSTANT_HEAD(:,:)
	    real, allocatable :: Cbb_RECHARGE(:,:)
	    real, allocatable :: Cbb_WELLS(:,:)
	    real, allocatable :: Cbb_DRAINS(:,:)
	    real, allocatable :: Cbb_CLN(:,:)
	    real, allocatable :: Cbb_SWF(:,:)
	    real, allocatable :: Cbb_FLOW_FACE(:,:)
	    real, allocatable :: Cbb_GWF(:,:)
	    real, allocatable :: Cbb_SWBC(:,:)
        
        real, allocatable :: laycbd(:)
        !real, allocatable :: bot(:)
        !real, allocatable :: top(:)
        
        ! GWF properties (cell-based)
        real, allocatable :: Kh(:)
        real, allocatable :: Kv(:)
        real, allocatable :: Ss(:)
        real, allocatable :: Sy(:)
        real, allocatable :: Alpha(:)
        real, allocatable :: Beta(:)
        real, allocatable :: Sr(:)
        real, allocatable :: Brooks(:)

        ! CLN properties (zoned)
        real, allocatable :: Length(:)             ! length of CLN
        real, allocatable :: LowestElevation(:)    ! lowest elevation of CLN
        real, allocatable :: SlopeAngle(:)         ! angle above horizontal of angled CLN
        integer, allocatable    :: Geometry(:)           ! circular or rectangular
        integer, allocatable    :: Direction(:)          ! vertical, horizontal or angled
        real, allocatable       :: CircularRadius(:)    ! dimension of CLN
        real, allocatable       :: RectangularWidth(:)    ! dimension of CLN
        real, allocatable       :: RectangularHeight(:)    ! dimension of CLN
        real, allocatable       :: LongitudinalK(:)    ! dimension of CLN
        integer, allocatable    :: FlowTreatment(:)       ! confined/unconfined, laminar/turbulent etc

        ! SWF properties (zoned)
        real, allocatable :: Sgcl(:)   ! SWF-GWF connection length
        real, allocatable :: Manning(:)   ! Manning's coefficient of friction
        real, allocatable :: DepressionStorageHeight(:)
        real, allocatable :: ObstructionStorageHeight(:)
        real, allocatable :: H1DepthForSmoothing(:)   ! SWF depth smoothing parameter
        real, allocatable :: H2DepthForSmoothing(:)   ! SWF depth smoothing parameter
            
    end type ModflowDomain
 

    type ModflowProject
 
        type(TecplotDomain) TMPLT
        type(TecplotDomain) TMPLT_GWF
        type(TecplotDomain) TMPLT_CLN
        type(TecplotDomain) TMPLT_SWF
        type(ModflowDomain) GWF
        type(ModflowDomain) CLN
        type(ModflowDomain) SWF
        
        character(128) :: MUTPrefix
        character(128) :: Prefix='Modflow'
        
        ! By default, 2D finite-elements in template mesh are used to define control volumes
        logical :: NodalControlVolume=.false.
        
        logical :: InnerCircles=.false.      ! Set to true if inner circles are calculated for SWF triangles

        logical :: TagFiles
        logical :: GenOCFile

        ! GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameGSF
        integer :: iGSF
        
        ! CLN_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameCLN_GSF
        integer :: iCLN_GSF
        
        ! SWF_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameSWF_GSF
        integer :: iSWF_GSF
        
        ! NAM file
        character(128) :: FNameNAM
        integer :: iNAM
        
        ! DISU file
        character(128) :: FNameDISU
        integer :: iDISU

        ! LIST file
        character(128) :: FNameLIST
        integer :: iLIST
        
        ! Units
        character(MAX_LBL) :: STR_TimeUnit
        integer :: TimeUnits=1    ! default 1 is seconds
        character(MAX_LBL) :: STR_LengthUnit
        integer :: LengthUnits=2   ! default 2 is meters
        
        ! BAS6 file
        character(128) :: FNameBAS6
        integer :: iBAS6
        ! BAS6 options 
        logical :: xsection=.false.
        logical :: chtoch=.false.
        logical :: free=.false.
        logical :: printtime=.false.
        logical :: unstructured=.false.
        logical :: printfv=.false.
        logical :: converge=.false.
        logical :: richards=.false.
        logical :: dpin=.false.
        logical :: dpout=.false.
        logical :: dpio=.false.
        logical :: ihm=.false.
        logical :: syall=.false.
        integer :: ixsec = 0    
        integer :: ichflg = 0   
        integer :: ifrefm = 0   
        integer :: iprtim = 0   
        integer :: iunstr = 0   
        integer :: iprconn = 0  
        integer :: ifrcnvg = 0 
        integer :: iunsat=1
        integer :: idpin = 0    
        integer :: idpout = 0   
        integer :: ihmsim = 0   
        integer :: iuihm = 0    
        integer :: isyall = 0   

        ! SMS file
        character(128) :: FNameSMS
        integer :: iSMS

        ! OC file
        character(128) :: FNameOC
        integer :: iOC
        integer :: ntime = 0
        real, allocatable :: timot(:)
        real(dr), allocatable :: OutputTimes(:)
        integer :: nOutputTimes
        
        !Stress Periods
        integer :: nPeriods = 0
        real, allocatable :: StressPeriodDuration(:)
        integer, allocatable :: StressPeriodnTsteps(:)
        real, allocatable :: StressPeriodnTstepMult(:)
        character(2), allocatable :: StressPeriodType(:)
        ! Stress period defaults
        real :: StressPeriodDeltat=1.000000e-03
        real :: StressPeriodTminat=1.000000e-05
        real :: StressPeriodTmaxat=60.0d0
        real :: StressPeriodTadjat=1.100000e+00
        real :: StressPeriodTcutat=2.000000e+00        
        
        ! LPF file
        character(128) :: FNameLPF
        integer :: iLPF

        ! RCH file
        character(128) :: FNameRCH
        integer :: iRCH
        
        ! RIV file
        character(128) :: FNameRIV
        integer :: iRIV
        
        ! WEL file
        character(128) :: FNameWEL
        integer :: iWEL
        
        ! CHD file
        character(128) :: FNameCHD
        integer :: iCHD

        ! EVT file
        character(128) :: FNameEVT
        integer :: iEVT
        
        ! DRN file
        character(128) :: FNameDRN
        integer :: iDRN
 
        ! CLN file
        character(128) :: FNameCLN
        integer :: iCLN

        ! SWF file
        character(128) :: FNameSWF
        integer :: iSWF

        ! SWBC file
        character(128) :: FNameSWBC
        integer :: iSWBC
       
        ! GNC file
        character(128) :: FNameGNC
        integer :: iGNC

        ! LAK file
        character(128) :: FNameLAK
        integer :: iLAK=0


        
        ! DATA(BINARY) files
        ! HDS file
        

        ! CBCCLN file
        character(128) :: FNameCBCCLN
        integer :: iCBCCLN
  
        ! Scan file
        integer :: nDim=10000
        integer :: nKeyWord
        character(MAX_STR), ALLOCATABLE :: KeyWord(:) ! read buffer for location data
        character(128) :: FNameSCAN
        integer :: iSCAN
        
        ! Modflow file extensions MUT currently does not recognize or process
        integer ::iBCF6
        integer ::iEVS 
        integer ::iGHB 
        integer ::iRTS 
        integer ::iTIB 
        integer ::iDPF 
        integer ::iPCB 
        integer ::iBCT 
        integer ::iFHB 
        integer ::iRES 
        integer ::iSTR 
        integer ::iIBS 
        integer ::iHFB6
        integer ::iDIS 
        integer ::iPVAL
        integer ::iSGB 
        integer ::iHOB 
        integer ::iDPT 
        integer ::iZONE
        integer ::iMULT
        integer ::iDROB
        integer ::iRVOB
        integer ::iGBOB
        integer ::iDDF 
        integer ::iCHOB
        integer ::iETS 
        integer ::iDRT 
        integer ::iQRT 
        integer ::iGMG 
        integer ::ihyd 
        integer ::iSFR 
        integer ::iMDT 
        integer ::iGAGE
        integer ::iLVDA
        integer ::iSYF 
        integer ::ilmt6
        integer ::iMNW1
        integer ::iKDEP
        integer ::iSUB 
        integer ::iUZF 
        integer ::igwm 
        integer ::iSWT 
        integer ::iPATH
        integer ::iPTH 
        integer ::iTVM 


        !character(128) :: Name
        !integer :: LengthName
        !logical :: Exists=.false.
	    !integer :: Unit

        ! logical :: blockel
        !
        !
        !logical,allocatable :: nchosen(:)
        !
        !integer :: iz
        !integer :: ic
        !
	    !real(dr), allocatable :: Kx(:)
	    !real(dr), allocatable :: Thick(:)
	    !real(dr), allocatable :: T(:)
	    !real(dr), allocatable :: Ky(:)
	    !real(dr), allocatable :: Kz(:)
	    !real(dr), allocatable :: Ss(:)					
	    !real(dr), allocatable :: Sy(:)
	    !real(dr), allocatable :: Vanis(:)
     !   
        ! River Flows
        integer :: nlines
	    integer, allocatable :: StressPeriod(:)
	    integer, allocatable :: RiverCell(:)
	    real(dr), allocatable :: RiverFlow(:)
	    real(dr), allocatable :: RiverHead(:)
	    real(dr), allocatable :: RiverElev(:)
	    real(dr), allocatable :: RiverCond(:)
	    real(dr), allocatable :: RiverConc(:)

        ! Head Calibration
        ! StressPeriod,WellName,X83_ft,Y89_ft,Zmin,Zmax,ZMidpoint,Observed_ft,Simulated_ft,Residual_ft,Residual_ft_Jeff
        integer :: nlinesHead
	    integer, allocatable :: StressPeriodHead(:)
	    character(30), allocatable :: WellNameHead(:)
	    real(dr), allocatable :: Xhead(:)
	    real(dr), allocatable :: YHead(:)
	    real(dr), allocatable :: ZminHead(:)
	    real(dr), allocatable :: ZmaxHead(:)
	    real(dr), allocatable :: ZMidpointHead(:)
 	    real(dr), allocatable :: Observed_ft(:)
 	    real(dr), allocatable :: Simulated_ft(:)
 	    real(dr), allocatable :: Residual_ft(:)
        
        ! Well Construction
        ! Name	X	Y	Bottom_elevation_ft	Top_elevation_ft	casing_radius_ft	Well_on	Well_off
        integer :: nWellConst
	    character(30), allocatable :: NameWellConst(:)
	    real(dr), allocatable :: XWellConst(:)
	    real(dr), allocatable :: YWellConst(:)
	    real(dr), allocatable :: BotElevWellConst(:)
	    real(dr), allocatable :: TopElevWellConst(:)
	    real(dr), allocatable :: CasingRadiusWellConst(:)
	    character(30), allocatable :: TonWellConst(:)
	    character(30), allocatable :: ToffWellConst(:)


        ! EI Well Construction
        ! Name	X	Y	Top_elevation_ft	L1   Offset  L2
        integer :: n_EIWell
	    character(30), allocatable :: Name_EIWell(:)
	    real(dr), allocatable :: X_EIWell(:)
	    real(dr), allocatable :: Y_EIWell(:)
	    real(dr), allocatable :: TopElev_EIWell(:)
	    real(dr), allocatable :: ScreenALength_EIWell(:)
	    real(dr), allocatable :: ScreenBOffset_EIWell(:)
	    real(dr), allocatable :: ScreenBLength_EIWell(:)
	    real(dr), allocatable :: ScreenCOffset_EIWell(:)
	    real(dr), allocatable :: ScreenCLength_EIWell(:)
        
        ! CLN file
        
      !! URWORD        
      !integer :: linlen
      !integer :: ncode, icol, iout, in
      !integer :: istart
      !real :: r
      !integer :: istop,n

        

    end type ModflowProject

    ! other local variables
    integer, Parameter :: MAXCLN=10000  ! assuming never more than 10000 CLN's
    integer, Parameter :: MAXSTRESS=10000  ! assuming never more than 10000 Stress Periods

    real :: MinSeparationDistance=0.0001
    
    
    contains

    !-------------------------------------------------------------
    subroutine AddCLNFiles(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
        
        ! Initialize CLN file and write data to NAM
        Modflow.FNameCLN=trim(Modflow.Prefix)//'.CLN'
        call OpenAscii(Modflow.iCLN,Modflow.FNameCLN)
        call Msg('  ')
        call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameCLN))
        write(Modflow.iNAM,'(a,i4,a)') 'CLN ',Modflow.iCLN,' '//trim(Modflow.FNameCLN)
        write(Modflow.iCLN,'(a,1pg10.1)') '# MODFLOW-USG CLN file written by Modflow-User-Tools version ',MUTVersion

        Modflow.CLN.FNameCBB=trim(Modflow.Prefix)//'.CLN.cbb'
        call getunit(Modflow.CLN.iCBB)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.CLN.iCBB,' '//trim(Modflow.CLN.FNameCBB)
        
        Modflow.CLN.FNameHDS=trim(Modflow.Prefix)//'.CLN.HDS'
        call getunit(Modflow.CLN.iHDS)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.CLN.iHDS,' '//trim(Modflow.CLN.FNameHDS)
        
        Modflow.CLN.FNameDDN=trim(Modflow.Prefix)//'.CLN.DDN'
        call getunit(Modflow.CLN.iDDN)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.CLN.iDDN,' '//trim(Modflow.CLN.FNameDDN)
    end subroutine AddCLNFiles
    
    !-------------------------------------------------------------
    subroutine AddSWFFiles(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
        
        
        ! Initialize SWF file and write data to NAM
        Modflow.FNameSWF=trim(Modflow.Prefix)//'.swf'
        call OpenAscii(Modflow.iSWF,Modflow.FNameSWF)
        call Msg('  ')
        call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameSWF))
        write(Modflow.iNAM,'(a,i4,a)') 'SWF ',Modflow.iSWF,' '//trim(Modflow.FNameSWF)
        write(Modflow.iSWF,'(a,1pg10.1)') '# MODFLOW-USG SWF file written by Modflow-User-Tools version ',MUTVersion

        
        Modflow.SWF.FNameCBB=trim(Modflow.Prefix)//'.SWF.cbb'
        call getunit(Modflow.SWF.iCBB)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.SWF.iCBB,' '//trim(Modflow.SWF.FNameCBB)
        
        Modflow.SWF.FNameHDS=trim(Modflow.Prefix)//'.SWF.HDS'
        call getunit(Modflow.SWF.iHDS)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.SWF.iHDS,' '//trim(Modflow.SWF.FNameHDS)
        
        Modflow.SWF.FNameDDN=trim(Modflow.Prefix)//'.SWF.DDN'
        call getunit(Modflow.SWF.iDDN)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.SWF.iDDN,' '//trim(Modflow.SWF.FNameDDN)
    end subroutine AddSWFFiles
    
    !-------------------------------------------------------------
    subroutine AddToScan(PKey, Modflow)
        implicit none
        
        
        type (ModflowProject) Modflow

        character(*) :: PKey
        
        !if(Modflow.nKeyWord .gt. 0) then
        !    if(Modflow.Keyword(Modflow.nKeyWord) .eq. PKey) then
        !        !write(Modflow.iSCAN,'(a)',iostat=status) 'Repeat: '// trim(PKey)
        !        return
        !    end if
        !end if
        
        Modflow.nKeyWord=Modflow.nKeyWord+1
        if(Modflow.nKeyWord>Modflow.nDim) call GrowKeywordArray(Modflow,Modflow.nDim)
        Modflow.Keyword(Modflow.nKeyWord)=PKey
        write(Modflow.iSCAN,'(a)',iostat=status) Modflow.Keyword(Modflow.nKeyWord)

        
        
    end subroutine AddToScan
    
    !----------------------------------------------------------------------
    subroutine AssignAlphatoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)//'^(-1)' 

		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Alpha of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Alpha(i)=value
            end if
        end do
    
    end subroutine AssignAlphatoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignBetatoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5)') value
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Beta of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Beta(i)=value
            end if
        end do
    
    end subroutine AssignBetatoDomain

    !----------------------------------------------------------------------
    subroutine AssignBrookstoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5)') value
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Brooks of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Brooks(i)=value
            end if
        end do
    
    end subroutine AssignBrookstoDomain

    !----------------------------------------------------------------------
    subroutine AssignCHDtoDomain(FNumMUT,modflow,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) modflow
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: head
        
        read(FNumMUT,*) head
        write(TmpSTR,'(a,g15.5,a)') TAB//'Assigning '//domain.name//' constant head: ',head,'     '//TRIM(UnitsOfLength) 
		call Msg(trim(TmpSTR))


        if(.not. allocated(domain.ConstantHead)) then 
            allocate(domain.ConstantHead(domain.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell constant head array')            
            domain.ConstantHead(:)=-999.d0
        end if
        
        call Msg(TAB//'    Cell    Constant head')
        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                call set(domain.Cell_Is(i),ConstantHead)
                domain.nCHDCells=domain.nCHDCells+1
                domain.ConstantHead(i)=head
                write(TmpSTR,'(i8,2x,g15.5,a)') i,domain.ConstantHead(i),'     '//TRIM(UnitsofLength)
                call Msg(TAB//trim(TmpSTR))
            end if
        end do
        
        if(modflow.iCHD == 0) then ! Initialize CHD file and write data to NAM
            Modflow.FNameCHD=trim(Modflow.Prefix)//'.chd'
            call OpenAscii(Modflow.iCHD,Modflow.FNameCHD)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameCHD))
            write(Modflow.iNAM,'(a,i4,a)') 'CHD  ',Modflow.iCHD,' '//trim(Modflow.FNameCHD)
            write(Modflow.iCHD,'(a,1pg10.1)') '# MODFLOW-USG CHD file written by Modflow-User-Tools version ',MUTVersion
        else
            pause 'next stress period?'
        end if

    end subroutine AssignCHDtoDomain

    !----------------------------------------------------------------------
    subroutine AssignCriticalDepthtoDomain(modflow,domain) 
        implicit none

        type (ModflowProject) modflow
        type (ModflowDomain) domain
        
        integer :: i, j, k, j1, j2, jNext, nNext, jLast, nLast
        
		call Msg(TAB//'Define all chosen '//trim(domain.name)//' Cells to be critical depth')

        call Msg('Assumes appropriate '//trim(domain.name)//' SWBC nodes flagged as boundary nodes') 
        
        if(Modflow.NodalControlVolume) then
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_is(i),CriticalDepth)
                    domain.nSWBCCells=domain.nSWBCCells+1
                    do k=1,domain.nElements
                        nLoop: do j=1,domain.nNodesPerCell
                            j1=domain.iNode(j,k)
                            if(j1==i) then
                                jNext=j+1
                                if(jNext>domain.nNodesPerCell) jNext=1
                                jLast=j-1
                                if(jLast<1) jLast=domain.nNodesPerCell
                                
                                nNext=domain.iNode(jNext,k)
                                if(bcheck(domain.Node_is(j1),BoundaryNode) .and. bcheck(domain.Node_is(nNext),BoundaryNode)) then ! add half sidelength to CriticalDepthLength
                                    domain.CriticalDepthLength(i)=domain.CriticalDepthLength(i)+domain.SideLength(j,k)/2.0d0
                                endif
                                
                                nLast=domain.iNode(jLast,k)
                                if(bcheck(domain.Node_is(j1),BoundaryNode) .and. bcheck(domain.Node_is(nLast),BoundaryNode)) then ! add half sidelength to CriticalDepthLength
                                    domain.CriticalDepthLength(i)=domain.CriticalDepthLength(i)+domain.SideLength(jLast,k)/2.0d0
                                endif
                                exit nLoop
                            endif
                        end do nLoop
                    end do
                end if
            end do
        else    
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_is(i),CriticalDepth)
                    domain.nSWBCCells=domain.nSWBCCells+1
                    do j=1,domain.nNodesPerCell
                        j1=domain.iNode(j,i)
                        if(j < domain.nNodesPerCell) then
                            j2=domain.iNode(j+1,i)
                        else
                            j2=domain.iNode(1,i)
                        end if
                        if(bcheck(domain.Node_is(j1),BoundaryNode) .and. bcheck(domain.Node_is(j2),BoundaryNode)) then ! add sidelength to CriticalDepthLength
                            domain.CriticalDepthLength(i)=domain.CriticalDepthLength(i)+domain.SideLength(j,i)
                        end if
                    end do
                end if
            end do
        end if
 
        if(Modflow.iSWBC == 0) then ! Initialize SWBC file and write data to NAM
            Modflow.FNameSWBC=trim(Modflow.Prefix)//'.swbc'
            call OpenAscii(Modflow.iSWBC,Modflow.FNameSWBC)
            call Msg('  ')
            call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameSWBC))
            write(Modflow.iNAM,'(a,i4,a)') 'SWBC  ',Modflow.iSWBC,' '//trim(Modflow.FNameSWBC)
            write(Modflow.iSWBC,'(a,1pg10.1)') '# MODFLOW-USG SWBC file written by Modflow-User-Tools version ',MUTVersion
        else
            pause 'next stress period?'
        end if


    end subroutine AssignCriticalDepthtoDomain    
    
    !----------------------------------------------------------------------
    subroutine AssignCriticalDepthtoCellsSide1(modflow,domain) 
        implicit none

        type (ModflowProject) modflow
        type (ModflowDomain) domain
        
        integer :: i
        
		call Msg(TAB//'Define all chosen '//trim(domain.name)//' Cells to be critical depth')

        call Msg(TAB//'Assumes SWBC Critical Depth Length equals sqrt(cell area)') 
        
        if(Modflow.NodalControlVolume) then
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_is(i),CriticalDepth)
                    domain.nSWBCCells=domain.nSWBCCells+1
                    domain.CriticalDepthLength(i)=SQRT(domain.cellarea(i))
                endif
            end do
        else    
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_is(i),CriticalDepth)
                    domain.nSWBCCells=domain.nSWBCCells+1
                    domain.CriticalDepthLength(i)=SQRT(domain.cellarea(i))
                    
                end if
            end do
        end if
 
        if(Modflow.iSWBC == 0) then ! Initialize SWBC file and write data to NAM
            Modflow.FNameSWBC=trim(Modflow.Prefix)//'.swbc'
            call OpenAscii(Modflow.iSWBC,Modflow.FNameSWBC)
            call Msg('  ')
            call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameSWBC))
            write(Modflow.iNAM,'(a,i4,a)') 'SWBC  ',Modflow.iSWBC,' '//trim(Modflow.FNameSWBC)
            write(Modflow.iSWBC,'(a,1pg10.1)') '# MODFLOW-USG SWBC file written by Modflow-User-Tools version ',MUTVersion
        else
            pause 'next stress period?'
        end if


    end subroutine AssignCriticalDepthtoCellsSide1

    !----------------------------------------------------------------------
    subroutine AssignDepthForSmoothingtoSWF(FnumMUT,domain)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value1, value2
        
        read(FNumMUT,*) value1, value2
        write(TmpSTR,'(1pg15.5,a)') value1,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(domain.name)//' H1 depth for smoothing: '//trim(TmpSTR))
        write(TmpSTR,'(1pg15.5,a)') value2,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(domain.name)//' H2 depth for smoothing: '//trim(TmpSTR))

        
        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                domain.H1DepthForSmoothing(i)=value1
                domain.H2DepthForSmoothing(i)=value2
            end if
        end do

    end subroutine AssignDepthForSmoothingtoSWF

    !----------------------------------------------------------------------
    subroutine AssignDRNtoDomain(FNumMUT,modflow,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) modflow
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: cond
        
        read(FNumMUT,*) cond
        write(TmpSTR,'(a,g15.5,a)') TAB//'Assigning '//trim(domain.name)//' drain conductance: ',cond,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
		call Msg(trim(TmpSTR))


        if(.not. allocated(domain.DrainConductance)) then 
            allocate(domain.DrainConductance(domain.nCells),domain.DrainElevation(domain.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell drain arrays')            
            domain.ConstantHead(:)=-999.d0
        end if
        
        call Msg('        Cell      DrainElevation             DrainConductance')
        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                call set(domain.Cell_Is(i),Drain)
                domain.nDRNCells=domain.nDRNCells+1
                domain.DrainElevation(i)=domain.Top(i)
                domain.DrainConductance(i)=cond
                write(TmpSTR,'(i8,2x,g15.5,a,g15.5,a)') i,domain.DrainElevation(i),'     '//TRIM(UnitsOfLength), domain.DrainConductance(i),'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
                call Msg(TAB//trim(TmpSTR))
            end if
        end do
        
        if(modflow.iDRN == 0) then ! Initialize DRN file and write data to NAM
            Modflow.FNameDRN=trim(Modflow.Prefix)//'.drn'
            call OpenAscii(Modflow.iDRN,Modflow.FNameDRN)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameDRN))
            write(Modflow.iNAM,'(a,i4,a)') 'DRN  ',Modflow.iDRN,' '//trim(Modflow.FNameDRN)
            write(Modflow.iDRN,'(a,1pg10.1)') '# MODFLOW-USG DRN file written by Modflow-User-Tools version ',MUTVersion
        else
            pause 'next stress period?'
        end if

    end subroutine AssignDRNtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignKhtoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Kh of '//trim(TmpSTR))
        
        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Kh(i)=value
            end if
        end do
    
    end subroutine AssignKhtoDomain
    !----------------------------------------------------------------------
    subroutine AssignKvtoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Kv of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Kv(i)=value
            end if
        end do
    
    end subroutine AssignKvtoDomain
   
    !----------------------------------------------------------------------
    subroutine AssignDepressiontoSWF(FnumMUT,domain)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(domain.name)//' Depression Storage Height: '//trim(TmpSTR))


        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                domain.DepressionStorageHeight(i)=value
            end if
        end do
    
    end subroutine AssignDepressiontoSWF

    !----------------------------------------------------------------------
    subroutine AssignManningtoSWF(FnumMUT,domain)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)//'^(-1/3)    '//TRIM(UnitsOfTime)
		call Msg(TAB//trim(domain.name)//' Manning''s coefficient of friction: '//trim(TmpSTR))


        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                domain.Manning(i)=value
            end if
        end do
    
    end subroutine AssignManningtoSWF

    !----------------------------------------------------------------------
    subroutine AssignObstructiontoSWF(FnumMUT,domain)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(domain.name)//' Obstruction Storage Height: '//trim(TmpSTR))


        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                domain.ObstructionStorageHeight(i)=value
            end if
        end do
    
    end subroutine AssignObstructiontoSWF

    !----------------------------------------------------------------------
    subroutine AssignMaterialtoGWF(FNumMUT, Domain) 
        implicit none
        
        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        integer :: iMaterial
        
        real :: LengthConversionFactor
        real :: TimeConversionFactor
        
        read(FNumMUT,*) iMaterial
        write(TmpSTR,'(g15.5)') iMaterial
        
        do i=1,nGWFMaterials
            if(iMaterial == GWF_MaterialID(i)) then
                iMaterial=i
                exit
            endif
        end do
        
        write(TmpSTR,'(i5)') iMaterial
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells properties of material '//trim(TmpSTR)//', '//trim(GWF_MaterialName(iMaterial)))
        
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Kh_Kx:             ',Kh_Kx(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'   '//TRIM(GWF_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Kv_Kz:             ',Kv_Kz(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'   '//TRIM(GWF_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Specific Storage:  ',SpecificStorage(iMaterial)  ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Specific Yield:    ',SpecificYield(iMaterial)    ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Alpha:             ',Alpha(iMaterial)            ,'     '//TRIM(GWF_LengthUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Beta:              ',Beta(iMaterial)             ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Sr:                ',Sr(iMaterial)               ,'     DIMENSIONLESS'
        call Msg(TmpSTR)
        select case(UnsaturatedFunctionType(iMaterial))
        case ('Van Genuchten')
            write(TmpSTR,'(a)')    TAB//'Unsaturated Function Type:   '//trim(UnsaturatedFunctionType(iMaterial))
            call Msg(TmpSTR)
        case ('Brooks-Corey')
            write(TmpSTR,'(a)')        TAB//'Unsaturated Function Type: '//trim(UnsaturatedFunctionType(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5)')TAB//'Brooks Corey Exponent:     ',BrooksCoreyExponent(iMaterial)
            call Msg(TmpSTR)
        case default
            call ErrMsg('Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported')
        end select
        
        LengthConversionFactor=LengthConverter(UnitsOfLength,GWF_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,GWF_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material length unit:    '//trim(GWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(GWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material time unit:    '//trim(GWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(GWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

       
        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                if(domain.name == 'GWF') then
                    domain.Kh(i)=Kh_Kx(iMaterial)*LengthConversionFactor/TimeConversionFactor       ! L/T
                    domain.Kv(i)=Kv_Kz(iMaterial)*LengthConversionFactor/TimeConversionFactor       ! L/T
                    domain.Ss(i)=Specificstorage(iMaterial)/LengthConversionFactor                  ! 1/L
                    domain.Sy(i)=SpecificYield(iMaterial)                                           ! -
                    domain.Alpha(i)=Alpha(iMaterial)/LengthConversionFactor                         ! 1/L
                    domain.Beta(i)=Beta(iMaterial)                                                  ! -
                    domain.Sr(i)=Sr(iMaterial)                                                      ! -
                    
                    select case(UnsaturatedFunctionType(iMaterial))
                    case ('Van Genuchten')
                        domain.Brooks(i)= -BrooksCoreyExponent(iMaterial)
                    case ('Brooks-Corey')
                        domain.Brooks(i)=BrooksCoreyExponent(iMaterial)
                    case default
                        call ErrMsg('Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported')
                    end select

                endif
            end if
        end do
        
        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
		    call Msg(TAB//'Properties of material '//trim(TmpSTR)//', '//trim(GWF_MaterialName(iMaterial))//' after unit conversion')
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Kh_Kx:             ',Kh_Kx(iMaterial)*LengthConversionFactor/TimeConversionFactor  ,'     '//TRIM(UnitsOfLength)//'   '//TRIM(UnitsOfTime)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Kv_Kz:             ',Kv_Kz(iMaterial)*LengthConversionFactor/TimeConversionFactor  ,'     '//TRIM(UnitsOfLength)//'   '//TRIM(UnitsOfTime)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Specific Storage:  ',SpecificStorage(iMaterial)/LengthConversionFactor             ,'     '//TRIM(UnitsOfLength)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Specific Yield:    ',SpecificYield(iMaterial)                                      ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Alpha:             ',Alpha(iMaterial)/LengthConversionFactor                       ,'     '//TRIM(UnitsOfLength)//'^(-1)'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Beta:              ',Beta(iMaterial)                                               ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Sr:                ',Sr(iMaterial)                                                 ,'     DIMENSIONLESS'
            call Msg(TmpSTR)
            select case(UnsaturatedFunctionType(iMaterial))
            case ('Van Genuchten')
                write(TmpSTR,'(a)')    TAB//'Unsaturated Function Type:   '//trim(UnsaturatedFunctionType(iMaterial))
                call Msg(TmpSTR)
            case ('Brooks-Corey')
                write(TmpSTR,'(a)')        TAB//'Unsaturated Function Type: '//trim(UnsaturatedFunctionType(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,1pg15.5)')TAB//'Brooks Corey Exponent:     ',BrooksCoreyExponent(iMaterial)
                call Msg(TmpSTR)
            case default
                call ErrMsg('Unsaturated Function Type '//trim(UnsaturatedFunctionType(iMaterial))//' not supported')
            end select

        end if     
        
      
        continue
    
    end subroutine AssignMaterialtoGWF
    
    real function LengthConverter(Project_LengthUnit,Material_LengthUnit)
        implicit none
        character(*) :: Project_LengthUnit
        character(*) :: Material_LengthUnit
        
        select case(Project_LengthUnit)
        case ('FEET')
            select case(Material_LengthUnit)
            case ('FEET')
                LengthConverter=1.0
            case ('METERS')
                LengthConverter=3.280839895
            case ('CENTIMETERS')
                LengthConverter=0.032808399
            end select

        case ('METERS')
            select case(Material_LengthUnit)
            case ('FEET')
                LengthConverter=0.3048
            case ('METERS')
                LengthConverter=1.0
            case ('CENTIMETERS')
                LengthConverter=0.01
            end select

        case ('CENTIMETERS')
            select case(Material_LengthUnit)
            case ('FEET')
                LengthConverter=30.48
            case ('METERS')
                LengthConverter=100.0
            case ('CENTIMETERS')
                LengthConverter=1.0
            end select
        end select
    
    end function LengthConverter
    
    real function TimeConverter(Project_TimeUnit,Material_TimeUnit)
        implicit none
        character(*) :: Project_TimeUnit
        character(*) :: Material_TimeUnit
        
        select case(Project_TimeUnit)
        case ('SECONDS')
            select case(Material_TimeUnit)
            case ('SECONDS')
                TimeConverter=1.0
            case ('MINUTES')
                TimeConverter=60.0
            case ('HOURS')
                TimeConverter=60.0*60.0
            case ('DAYS')
                TimeConverter=60.0*60.0*24.0
            case ('YEARS')
                TimeConverter=60.0*60.0*24.0*365.0
            end select
        case ('MINUTES')
            select case(Material_TimeUnit)
            case ('SECONDS')
                TimeConverter=1.0/60.0
            case ('MINUTES')
                TimeConverter=1.0
            case ('HOURS')
                TimeConverter=60.0
            case ('DAYS')
                TimeConverter=60.0*24.0
            case ('YEARS')
                TimeConverter=60.0*24.0*365.0
            end select
        case ('HOURS')
            select case(Material_TimeUnit)
            case ('SECONDS')
                TimeConverter=1.0/60.0/60.0
            case ('MINUTES')
                TimeConverter=1.0/60.0
            case ('HOURS')
                TimeConverter=1.0
            case ('DAYS')
                TimeConverter=24.0
            case ('YEARS')
                TimeConverter=24.0*365.0
            end select
        case ('DAYS')
            select case(Material_TimeUnit)
            case ('SECONDS')
                 TimeConverter=1.0/60.0/60.0/24.0
            case ('MINUTES')
                 TimeConverter=1.0/60.0/24.0
            case ('HOURS')
                 TimeConverter=1.0/24.0
            case ('DAYS')
                TimeConverter=1.0
            case ('YEARS')
                TimeConverter=365.0
            end select
        case ('YEARS')
            select case(Material_TimeUnit)
            case ('SECONDS')
                 TimeConverter=1.0/60.0/60.0/24.0/365.0
            case ('MINUTES')
                 TimeConverter=1.0/60.0/24.0/365.0
            case ('HOURS')
                 TimeConverter=1.0/24.0/365.0
            case ('DAYS')
                 TimeConverter=1.0/365.0
            case ('YEARS')
                TimeConverter=1.0
            end select
        end select
    
    end function TimeConverter
    !----------------------------------------------------------------------
    subroutine AssignMaterialtoCLN(FnumMUT,CLN)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) CLN
        
        integer :: i
        integer :: iMaterial

        real :: LengthConversionFactor
        real :: TimeConversionFactor

        read(FNumMUT,*) iMaterial
        write(TmpSTR,'(i5)') iMaterial
        
		call Msg(TAB//'Assigning all chosen '//trim(CLN.name)//' zones properties of material '//trim(TmpSTR)//', '//trim(CLN_Name(iMaterial)))
        select case(Geometry(iMaterial))
        case ('Circular')
            write(TmpSTR,'(a)')        TAB//'Geometry:           '//trim(Geometry(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Circular Radius:    ',CircularRadius(iMaterial)     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        case ('Rectangular')
            write(TmpSTR,'(a)')        TAB//'Geometry:           '//trim(Geometry(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Rectangular Width:  ',RectangularWidth(iMaterial)     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Rectangular Height: ',RectangularHeight(iMaterial)    ,'     '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        case default
            call ErrMsg('Geometry type '//trim(Geometry(iMaterial))//' not supported')
        end select

        write(TmpSTR,'(a)')            TAB//'Direction:          '//Direction(iMaterial)
        call Msg(TmpSTR)
        
        write(TmpSTR,'(a)')            TAB//'Flow Treatment:     '//FlowTreatment(iMaterial) 
        call Msg(TmpSTR)

        write(TmpSTR,'(a,1pg15.5,a)')    TAB//'Longitudinal K:     ',LongitudinalK(iMaterial)        ,'     '//TRIM(CLN_LengthUnit(iMaterial))//'   '//TRIM(CLN_TimeUnit(iMaterial))//'^(-1)'
        call Msg(TmpSTR)
        
        LengthConversionFactor=LengthConverter(UnitsOfLength,CLN_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,CLN_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material length unit:    '//trim(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(CLN_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material time unit:    '//trim(CLN_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(CLN_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

      
        do i=1,CLN.nZones
            if(bcheck(CLN.Zone_is(i),chosen)) then
                select case(Geometry(iMaterial))
                case ('Circular')
                    CLN.Geometry(i)=1
                    CLN.NCONDUITYP=CLN.NCONDUITYP+1
                case ('Rectangular')
                    CLN.Geometry(i)=2
                    CLN.NRECTYP=CLN.NRECTYP+1
                case default
                    call ErrMsg('Geometry type '//trim(Geometry(iMaterial))//' not supported')
                end select

                select case(Direction(iMaterial))
                case ('Vertical')
                    CLN.Direction(i)=0
                case ('Horizontal')
                    CLN.Direction(i)=1
                case ('Angled')
                    CLN.Direction(i)=2
                case default
                    call ErrMsg('Direction type '//trim(Direction(iMaterial))//' not supported')
                end select
                
                select case(FlowTreatment(iMaterial))
                case ('Confined\Laminar')
                    CLN.FlowTreatment(i)=1
                case ('Confined\Darcy-Weisbach')
                    CLN.FlowTreatment(i)=2
                case ('Confined\Heizen-Williams')
                    CLN.FlowTreatment(i)=3
                case ('Confined/Mannings')
                    CLN.FlowTreatment(i)=4
                case ('Unconfined\Laminar')
                    CLN.FlowTreatment(i)=-1
                case ('Unconfined\Darcy-Weisbach')
                    CLN.FlowTreatment(i)=-2
                case ('Unconfined\Heizen-Williams')
                    CLN.FlowTreatment(i)=-3
                case ('Unconfined/Mannings')
                    CLN.FlowTreatment(i)=-4
                case default
                    call ErrMsg('Flow treatment type '//trim(FlowTreatment(iMaterial))//' not supported')
                end select
                
                CLN.CircularRadius(i)=CircularRadius(iMaterial)*LengthConversionFactor         ! L
                CLN.RectangularWidth(i)=RectangularWidth(iMaterial)*LengthConversionFactor         ! L
                CLN.RectangularHeight(i)=RectangularHeight(iMaterial)*LengthConversionFactor         ! L
                CLN.LongitudinalK(i)=LongitudinalK(iMaterial)*LengthConversionFactor/TimeConversionFactor         ! L/T
            end if
        end do
        
        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
		    call Msg(TAB//'Properties of material '//trim(TmpSTR)//', '//trim(CLN_Name(iMaterial))//' after unit conversion')
            select case(Geometry(iMaterial))
            case ('Circular')
                write(TmpSTR,'(a)')        TAB//'Geometry:           '//trim(Geometry(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,1pg15.5,a)')TAB//'Circular Radius:    ',CircularRadius(iMaterial)*LengthConversionFactor      ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
            case ('Rectangular')
                write(TmpSTR,'(a)')        TAB//'Geometry:           '//trim(Geometry(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,1pg15.5,a)')TAB//'Rectangular Width:  ',RectangularWidth(iMaterial)*LengthConversionFactor      ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
                write(TmpSTR,'(a,1pg15.5,a)')TAB//'Rectangular Height: ',RectangularHeight(iMaterial)*LengthConversionFactor     ,'     '//TRIM(CLN_LengthUnit(iMaterial))
                call Msg(TmpSTR)
            case default
                call ErrMsg('Geometry type '//trim(Geometry(iMaterial))//' not supported')
            end select

            write(TmpSTR,'(a)')            TAB//'Direction:          '//Direction(iMaterial)
            call Msg(TmpSTR)
        
            write(TmpSTR,'(a)')            TAB//'Flow Treatment:     '//FlowTreatment(iMaterial) 
            call Msg(TmpSTR)

            write(TmpSTR,'(a,1pg15.5)')    TAB//'Longitudinal K:     ',LongitudinalK(iMaterial)*LengthConversionFactor/TimeConversionFactor        ,'     '//TRIM(CLN_LengthUnit(iMaterial))//'   '//TRIM(CLN_TimeUnit(iMaterial))//'^(-1)'
            call Msg(TmpSTR)

        end if     
    
    end subroutine AssignMaterialtoCLN
    
    !----------------------------------------------------------------------
    subroutine AssignMaterialtoSWF(FnumMUT,domain)
        implicit none


        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        integer :: iMaterial

        real :: LengthConversionFactor
        real :: TimeConversionFactor
        
        read(FNumMUT,*) iMaterial
        write(TmpSTR,'(i5)') iMaterial
        
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' zones properties of material '//trim(TmpSTR)//', '//trim(SWF_MaterialName(iMaterial)))
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Manning''s Coefficient:      ',ManningCoefficient(iMaterial)     ,'     '//TRIM(SWF_LengthUnit(iMaterial))//'^(-1/3)  '//TRIM(SWF_TimeUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Depression Storage Height:  ',DepressionStorageHeight(iMaterial) ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'Obstruction Storage Height: ',ObstructionStorageHeight(iMaterial),'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'SWF Smoothing Depth 1:      ',SWFSmoothingDepth1(iMaterial)      ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'SWF Smoothing Depth 2:      ',SWFSmoothingDepth2(iMaterial)      ,'     '//TRIM(SWF_LengthUnit(iMaterial))
        call Msg(TmpSTR)

                
        LengthConversionFactor=LengthConverter(UnitsOfLength,SWF_LengthUnit(iMaterial))
        TimeConversionFactor=TimeConverter(UnitsOfTime,SWF_TimeUnit(iMaterial))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material length unit:    '//trim(SWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(SWF_LengthUnit(iMaterial))
            call Msg(TmpSTR)
        endif
            
         if(TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Time Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'Material time unit:    '//trim(SWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow time unit:     '//trim(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Time Conversion Factor:     ',TimeConversionFactor,'     '//TRIM(UnitsOfTime)//' per '//TRIM(SWF_TimeUnit(iMaterial))
            call Msg(TmpSTR)
        endif

       

        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                domain.Manning(i)=ManningCoefficient(iMaterial)*LengthConversionFactor**(-1/3)*TimeConversionFactor ! L^(-1/3) T
                domain.DepressionStorageHeight(i)=DepressionStorageHeight(iMaterial)*LengthConversionFactor         ! L
                domain.ObstructionStorageHeight(i)=ObstructionStorageHeight(iMaterial)*LengthConversionFactor       ! L
                domain.H1DepthForSmoothing(i)=SWFSmoothingDepth1(iMaterial)*LengthConversionFactor                  ! L
                domain.H2DepthForSmoothing(i)=SWFSmoothingDepth2(iMaterial)*LengthConversionFactor                  ! L
            end if                                                                               
        end do                                                                                   

        if(LengthConversionFactor /= 1.0 .OR. TimeConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iMaterial
		    call Msg(TAB//'Properties of material '//trim(TmpSTR)//', '//trim(SWF_MaterialName(iMaterial))//' after unit conversion')
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Manning''s Coefficient:      ',ManningCoefficient(iMaterial)*LengthConversionFactor**(-1/3)*TimeConversionFactor      ,'     '//TRIM(UnitsOfLength)//'^(-1/3)  '//TRIM(UnitsOfTime)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Depression Storage Height:  ',DepressionStorageHeight(iMaterial)*LengthConversionFactor ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Obstruction Storage Height: ',ObstructionStorageHeight(iMaterial)*LengthConversionFactor,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'SWF Smoothing Depth 1:      ',SWFSmoothingDepth1(iMaterial)*LengthConversionFactor      ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'SWF Smoothing Depth 2:      ',SWFSmoothingDepth2(iMaterial)      ,'     '//TRIM(UnitsOfLength)
            call Msg(TmpSTR)

        end if     

    end subroutine AssignMaterialtoSWF

    !----------------------------------------------------------------------
    subroutine AssignRCHtoDomain(FNumMUT,modflow,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) modflow
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: rech
        integer :: nRCHoption
        
        read(FNumMUT,*) rech
        write(TmpSTR,'(a,g15.5,a)') TAB//'Assigning '//domain.name//' recharge: ',rech,'     '//TRIM(modflow.STR_LengthUnit)//'   '//TRIM(modflow.STR_Timeunit)//'^(-1)'
		call Msg(trim(TmpSTR))
        read(FNumMUT,*) nRCHoption
        write(TmpSTR,'(a,g15.5)') TAB//'Assigning '//domain.name//' recharge option: ',nRCHoption
		call Msg(trim(TmpSTR))
        domain.nRCHoption=nRCHoption
        IF(nRCHoption.EQ.1) then
            call Msg(TAB//'Option 1 -- recharge to top layer')
        else IF(nRCHoption.EQ.2) then
            call Msg(TAB//'option 2 -- recharge to one specified node in each vertical column') 
        else IF(nRCHoption.EQ.3) then
            call Msg(TAB//'Option 3 -- recharge to highest active node in each vertical column')
        else IF(nRCHoption.EQ.4) then
            call Msg(TAB//'Option 4 -- recharge to swf domain on top of each vertical column')
        endif

        
        if(.not. allocated(domain.Recharge)) then ! 
            allocate(domain.Recharge(domain.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell recharge array')            
            domain.Recharge(:)=-999.d0
        end if
        
        do i=1,domain.nCells
            call set(domain.Cell_Is(i),Recharge)
            domain.Recharge(i)=rech
        end do

        if(modflow.iRCH == 0) then ! Initialize RCH file and write data to NAM
            Modflow.FNameRCH=trim(Modflow.Prefix)//'.rch'
            call OpenAscii(Modflow.iRCH,Modflow.FNameRCH)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameRCH))
            write(Modflow.iNAM,'(a,i4,a)') 'RCH  ',Modflow.iRCH,' '//trim(Modflow.FNameRCH)
            write(Modflow.iRCH,'(a,1pg10.1)') '# MODFLOW-USG RCH file written by Modflow-User-Tools version ',MUTVersion
            write(Modflow.iRCH,*) domain.nRCHoption, domain.iCBB  
            write(modflow.iRCH,*) 1   ! inrech, defaults to read one layer of recharge values
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain.name == 'GWF') then
                do i=1,domain.nCells
                    if(Modflow.GWF.iLayer(i)==1) then
                        write(Modflow.iRCH,'(1pg15.5)') domain.recharge(i)
                    endif
                end do
            else if(domain.name == 'SWF') then
                write(Modflow.iRCH,'(5(1pg15.5))') (domain.recharge(i),i=1,domain.nCells)
            endif                


        else
            write(modflow.iRCH,*) 1   ! inrech, defaults to read one layer of recharge values
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain.name == 'GWF') then
                do i=1,domain.nCells
                    if(Modflow.GWF.iLayer(i)==1) then
                        write(Modflow.iRCH,'(1pg15.5)') domain.recharge(i)
                    endif
                end do
            else if(domain.name == 'SWF') then
                write(Modflow.iRCH,'(5(1pg15.5))') (domain.recharge(i),i=1,domain.nCells)
            endif                
        end if
    end subroutine AssignRCHtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignWELtoDomain(FNumMUT,modflow,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) modflow
        type (ModflowDomain) Domain
        
        integer :: i, itmp, itmpcln
        real(dr) :: PumpRate
        
        read(FNumMUT,*) PumpRate
        write(TmpSTR,'(a,g15.5,a)') TAB//'Assigning '//domain.name//' PumpingRate: ',PumpRate,'     '//TRIM(modflow.STR_LengthUnit)//'   '//TRIM(modflow.STR_Timeunit)//'^(-1)'
		call Msg(trim(TmpSTR))
        
        if(.not. allocated(domain.PumpingRate)) then ! 
            allocate(domain.PumpingRate(domain.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell PumpingRate array')            
            domain.PumpingRate(:)=-999.d0
        end if

        itmp=0
        itmpcln=0

        if(domain.name == 'GWF') then
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_Is(i),Well)
                    domain.PumpingRate(i)=PumpRate
                    itmp=itmp+1
                endif
            end do
        else if(domain.name == 'CLN') then
            do i=1,domain.nCells
                if(bcheck(domain.Cell_is(i),chosen)) then
                    call set(domain.Cell_Is(i),Well)
                    domain.PumpingRate(i)=PumpRate
                    itmpcln=itmpcln+1
                endif
            end do
        endif    
        

        if(modflow.iWEL == 0) then ! Initialize WEL file and write data to NAM
            Modflow.FNameWEL=trim(Modflow.Prefix)//'.WEL'
            call OpenAscii(Modflow.iWEL,Modflow.FNameWEL)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameWEL))
            write(Modflow.iNAM,'(a,i4,a)') 'WEL  ',Modflow.iWEL,' '//trim(Modflow.FNameWEL)
            write(Modflow.iWEL,'(a,1pg10.1)') '# MODFLOW-USG WEL file written by Modflow-User-Tools version ',MUTVersion
            if(itmp>0 .AND. itmpcln>0) then
                call ErrMsg('WEL package can currently only be used for GWF or CLN domains but not both at the same time')
            else if(itmp>0) then
                write(Modflow.iWEL,'(2i8)') itmp, modflow.GWF.icbb
                write(Modflow.iWEL,'(3i8)') itmp, 0, 0 
            else if(itmpcln>0) then
                write(Modflow.iWEL,'(2i8)') itmpcln, modflow.CLN.icbb
                write(Modflow.iWEL,'(3i8)') 0, 0, itmpcln 
            endif
                
            if(domain.name == 'GWF') then
                do i=1,domain.nCells
                    if(bcheck(domain.Cell_is(i),Well)) then
                        write(Modflow.iWEL,'(i8,pg15.5,i8)') i,domain.PumpingRate(i),0
                    endif
                end do
            else if(domain.name == 'CLN') then
                do i=1,domain.nCells
                    if(bcheck(domain.Cell_is(i),Well)) then
                        write(Modflow.iWEL,'(i8,pg15.5,i8)') i,domain.PumpingRate(i),0
                    endif
                end do
            endif
        else
            pause 'next stress period?'
        end if
    
    end subroutine AssignWELtoDomain
  !  !----------------------------------------------------------------------
  !  subroutine AssignWELtoDomain(FNumMUT,modflow,domain) 
  !      implicit none
  !
  !      integer :: FNumMUT
  !      type (ModflowProject) modflow
  !      type (ModflowDomain) Domain
  !      
  !      integer :: i
  !      real(dr) :: PumpRate
  !      
  !      read(FNumMUT,*) PumpRate
  !      write(TmpSTR,'(a,g15.5,a)') TAB//'Assigning '//domain.name//' pumping rate: ',PumpRate,'     '//TRIM(UnitsOfLength)//'^(3)     '//TRIM(UnitsOfTime)
		!call Msg(trim(TmpSTR))
  !
  !
  !      if(.not. allocated(domain.PumpingRate)) then 
  !          allocate(domain.PumpingRate(domain.nCells),stat=ialloc)
  !          call AllocChk(ialloc,'Cell pumping rate array')            
  !          domain.PumpingRate(:)=-999.d0
  !      end if
  !      
  !      call Msg(TAB//'    Cell    Pumping Rate')
  !      do i=1,domain.nCells
  !          if(bcheck(domain.Cell_is(i),chosen)) then
  !              call set(domain.Cell_Is(i),Well)
  !              domain.nWELCells=domain.nWELCells+1
  !              domain.PumpingRate(i)=PumpRate
  !              write(TmpSTR,'(i8,2x,g15.5,a)') i,domain.PumpingRate(i),'     '//TRIM(UnitsOfLength)//'^(3)     '//TRIM(UnitsOfTime)
  !              call Msg(TAB//trim(TmpSTR))
  !          end if
  !      end do
  !      
  !      if(modflow.iWEL == 0) then ! Initialize WEL file and write data to NAM
  !          Modflow.FNameWEL=trim(Modflow.Prefix)//'.WEL'
  !          call OpenAscii(Modflow.iWEL,Modflow.FNameWEL)
  !          call Msg('  ')
  !          call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameWEL))
  !          write(Modflow.iNAM,'(a,i4,a)') 'WEL  ',Modflow.iWEL,' '//trim(Modflow.FNameWEL)
  !          write(Modflow.iWEL,'(a,1pg10.1)') '# MODFLOW-USG WEL file written by Modflow-User-Tools version ',MUTVersion
  !      else
  !          pause 'next stress period?'
  !      end if
  !
  !  end subroutine AssignWELtoDomain

    
    !----------------------------------------------------------------------
    subroutine CLN_AssignCircularRadius(FnumMUT,CLN)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) CLN
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(CLN.name)//' CLN circular radius: '//trim(TmpSTR))


        do i=1,CLN.nZones
            if(bcheck(CLN.Zone_is(i),chosen)) then
                CLN.CircularRadius(i)=value
            end if
        end do
    
    end subroutine CLN_AssignCircularRadius
    !----------------------------------------------------------------------
    subroutine CLN_AssignRectangularWidthHeight(FnumMUT,CLN)
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) CLN
        
        integer :: i
        real(dr) :: width, height
        
        read(FNumMUT,*) width, height
        write(TmpSTR,'(2g15.5,a)')  width, height,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//trim(CLN.name)//' CLN rectangular width and height: '//trim(TmpSTR))


        do i=1,CLN.nZones
            if(bcheck(CLN.Zone_is(i),chosen)) then
                CLN.RectangularWidth(i)=width
                CLN.RectangularHeight(i)=height
            end if
        end do
    
    end subroutine CLN_AssignRectangularWidthHeight
    !----------------------------------------------------------------------
    subroutine AssignSgcltoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells an Sgcl of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Sgcl(i)=value
            end if
        end do
    
    end subroutine AssignSgcltoDomain

    !----------------------------------------------------------------------
    subroutine AssignSrtoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5)') value
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Sr of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Sr(i)=value
            end if
        end do
    
    end subroutine AssignSrtoDomain

    !----------------------------------------------------------------------
    subroutine AssignSstoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)//'^(-1)'
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Ss of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Ss(i)=value
            end if
        end do
    
    end subroutine AssignSstoDomain

    !----------------------------------------------------------------------
    subroutine AssignStartingDepthtoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a starting depth of '//trim(TmpSTR))


        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.StartingHeads(i)=domain.zCell(i)+value
            end if
        end do
    
    end subroutine AssignStartingDepthtoDomain

    !----------------------------------------------------------------------
    subroutine AssignStartingHeadtoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5,a)') value,'     '//TRIM(UnitsOfLength)
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells starting heads of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.StartingHeads(i)=value
            end if
        end do
    
    end subroutine AssignStartingHeadtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignSytoDomain(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        
        integer :: i
        real(dr) :: value
        
        read(FNumMUT,*) value
        write(TmpSTR,'(g15.5)') value
		call Msg(TAB//'Assigning all chosen '//trim(domain.name)//' cells a Sy of '//trim(TmpSTR))

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.Sy(i)=value
            end if
        end do
    
    end subroutine AssignSytoDomain

    !-------------------------------------------------------------
    subroutine BuildModflowUSG(FNumMUT, Modflow,prefix) !--- Build Modflow USG data structure from instructions
        implicit none

        character(MAX_STR) :: FName
        integer :: FNumMUT
        character(*) :: prefix
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_GWF
        type (TecplotDomain) TMPLT_SWF
        type (TecplotDomain) TMPLT_CLN
        
        integer :: i
        
        Modflow.MUTPrefix=prefix

        read(FNumMUT,'(a)',iostat=status,end=10) instruction
            
        if(index(instruction, ModflowPrefix_CMD)  /= 0) then
            read(FnumMUT,'(a)') Modflow.prefix
            call Msg(TAB//trim(Modflow.Prefix))
        else 
            backspace(FNumMUT)
        endif
        call InitializeModflowFiles(Modflow)

        do
            
            read(FNumMUT,'(a)',iostat=status,end=10) instruction
    
            if(index(instruction, 'end')  /= 0) then
                if(JustBuilt) then ! Modflow project file creation
                    call ModflowTMPLTScatterToTecplot(Modflow,TMPLT)                

                    if(modflow.GWF.nCells >0) then
                        call WriteGWFFiles(Modflow,TMPLT_GWF)
                        call GWFToTecplot(Modflow,TMPLT_GWF)
                        call ModflowDomainScatterToTecplot(Modflow,Modflow.GWF)  
                    end if
                    
                   
                    if(modflow.SWF.nCells >0) then
                        call WriteSWFFiles(Modflow,TMPLT_SWF)
                        call SWFToTecplot(Modflow,TMPLT_SWF)
                        call ModflowDomainScatterToTecplot(Modflow,Modflow.SWF)                
                    end if
                
                    if(modflow.CLN.nCells >0) then
                        call WriteCLNFiles(Modflow,TMPLT_CLN)
                        call CLNToTecplot(Modflow,TMPLT_CLN)
                        call ModflowDomainScatterToTecplot(Modflow,Modflow.CLN)                
                    end if
                    
                    if(modflow.iCHD>0) call WriteCHDFile(Modflow)
                    if(modflow.iDRN>0) call WriteDRNFile(Modflow)
                    !if(modflow.iWEL>0) call WriteWELFile(Modflow)
                    
                    ! Default boundary conditions for extra stress periods
                    if(Modflow.nPeriods>1) then
                        do i=2,Modflow.nPeriods
                            if(modflow.iCHD>0) write(modflow.iCHD,'(i10)') -1
                            if(modflow.iDRN>0) write(modflow.iDRN,'(i10)') -1
                            if(modflow.iRCH>0) write(modflow.iRCH,'(i10)') -1
                            if(modflow.iWel>0) write(modflow.iWel,'(3(i10))') -1, 0 -1
                            if(modflow.iSWBC>0) write(modflow.iSWBC,'(i10)') -1
                        end do
                     end if   
                    
                end if
                
                
                exit
            else
                call Msg(' ')
                call Msg(instruction)
            end if


            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.MUSG'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
            end if
            
            
            ! ========================================================================
            ! Pre-processing instructions 
            

            if(index(instruction, NodalControlVolumes_CMD)  /= 0) then
                ! Generate node centred control volume domains (default is mesh centred)
                Modflow.NodalControlVolume=.true.
                call Msg(TAB//'*** Control volumes (i.e. modflow cells) will be centred at 2D mesh nodes')

            ! Units set assignment
            else if(index(instruction, UnitsTime_CMD)  /= 0) then
                call UnitsTime(FnumMUT,Modflow)
            else if(index(instruction, UnitsLength_CMD)  /= 0) then
                call UnitsLength(FnumMUT,Modflow)
                
                
                
            ! SMS parameter set assignment
            else if(index(instruction, SMS_Database_CMD)  /= 0) then
                read(FnumMUT,'(a)') FName
                call GET_ENVIRONMENT_VARIABLE('USERBIN',USERBIN)
                call DB_ReadSMS(trim(USERBIN)//'\'//trim(FName)) 
            else if(index(instruction, SMSParamterSetNumber_CMD)  /= 0) then
                call SMSParamterSetNumber(FnumMUT)
            
            else if(index(instruction, GWFMaterialsDatabase_CMD)  /= 0) then
                read(FnumMUT,'(a)') FName
                call GET_ENVIRONMENT_VARIABLE('USERBIN',USERBIN)
                call DB_ReadGWFMaterials(trim(USERBIN)//'\'//trim(FName)) 

            else if(index(instruction, CLNMaterialsDatabase_CMD)  /= 0) then
                read(FnumMUT,'(a)') FName
                call GET_ENVIRONMENT_VARIABLE('USERBIN',USERBIN)
                call DB_ReadCLNMaterials(trim(USERBIN)//'\'//trim(FName)) 
            
            else if(index(instruction, SWFMaterialsDatabase_CMD)  /= 0) then
                read(FnumMUT,'(a)') FName
                call GET_ENVIRONMENT_VARIABLE('USERBIN',USERBIN)
                call DB_ReadSWFMaterials(trim(USERBIN)//'\'//trim(FName)) 
            
            else if(index(instruction, ET_Database_CMD)  /= 0) then
                read(FnumMUT,'(a)') FName
                call GET_ENVIRONMENT_VARIABLE('USERBIN',USERBIN)
                call DB_ReadET(trim(USERBIN)//'\'//trim(FName)) 
                
            else if(index(instruction, MeshFromGb_CMD)  /= 0) then
                ! Build the 2D template mesh from a grdbldr 2D mesh

                call MeshFromGb(FnumMUT,TMPLT)
                call TemplateToTecplot(Modflow,TMPLT)
                call IaJa_FromTecplot(TMPLT)

            else if(index(instruction, GenerateUniformRectangles_CMD)  /= 0) then
                ! Build the 2D template mesh from a simple 2D rectangular mesh
                call GenerateUniformRectangles(FnumMUT,TMPLT)
                call TemplateToTecplot(Modflow,TMPLT)
                call IaJa_FromTecplot(TMPLT)
                JustBuilt=.true.
            
            else if(index(instruction, QuadtreeMeshFromGWV_CMD)  /= 0) then
                ! Build the 2D template mesh from a grdbldr 2D mesh
                call Quadtree2DMeshFromGWV(FnumMUT,TMPLT)
                call TemplateToTecplot(Modflow,TMPLT)
                call IaJa_FromTecplot(TMPLT)
            
            else if(index(instruction, GenerateSWFDomain_CMD)  /= 0) then
                call GenerateSWFDomain(FnumMUT,TMPLT,TMPLT_SWF)
                call BuildModflowSWFDomain(Modflow,TMPLT,TMPLT_SWF)
                call AddSWFFiles(Modflow)

                JustBuilt=.true.
            
            else if(index(instruction, GenerateCLNDomain_CMD)  /= 0) then
                call GenerateCLNDomain(FnumMUT,TMPLT_CLN)
                !call CLN_IaJaStructure(TMPLT_CLN)
                call BuildModflowCLNDomain(Modflow,TMPLT,TMPLT_CLN)
                call AddCLNFiles(Modflow)
                !JustBuilt=.true.

            else if(index(instruction, GenerateLayeredGWFDomain_CMD)  /= 0) then
                call GenerateLayeredGWFDomain(FnumMUT,TMPLT,TMPLT_GWF)
                if(Modflow.NodalControlVolume) then
                    call Msg('Build node-centred control volume ia,ja from SWF domain')
                    call NodeCentredSWFIaJaStructureToGWF(TMPLT_SWF,TMPLT_GWF)
                else
                    call IaJa_FromTecplot(TMPLT_GWF)
                end if
                call BuildModflowGWFDomain(Modflow,TMPLT,TMPLT_GWF)
                JustBuilt=.true.
           
            else if(index(instruction, HGSToModflowStructure_CMD)  /= 0) then
                ! Add components to Modflow data structure from an existing HGS model
                call HGSToModflowStructure(FnumMUT,Modflow,TMPLT_GWF,TMPLT_SWF)
                call IaJa_FromTecplot(TMPLT_GWF)
                JustBuilt=.true.


            else if(index(instruction, ActiveDomain_CMD)  /= 0) then
                read(FNumMUT,'(a)') ActiveDomainSTR
                call LwrCse(ActiveDomainSTR)
                select case(ActiveDomainSTR)
                case ('TMPLT')
                    ActiveDomain=iTMPLT
                case ('gwf')
                    ActiveDomain=iGWF
                case ('swf')
                    ActiveDomain=iSWF
                case ('cln')
                    ActiveDomain=iCLN
                case default
                    call ErrMsg('Domain type '//trim(ActiveDomainSTR)//' not supported')
                end select
                call Msg(TAB//trim(ActiveDomainSTR))
                

            else if(index(instruction, ChooseAllNodes_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllNodes(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseAllNodes(modflow.GWF)
                case (iSWF)
                    call ChooseAllNodes(modflow.SWF)
                case (iCLN)
                    call ChooseAllNodes(modflow.CLN)
                end select

             else if(index(instruction, ClearAllNodes_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ClearAllNodes(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ClearAllNodes(modflow.GWF)
                case (iSWF)
                    call ClearAllNodes(modflow.SWF)
                case (iCLN)
                    call ClearAllNodes(modflow.CLN)
                end select
             
             else if(index(instruction, ChooseNodeAtXYZ_CMD)  /= 0) then
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

             else if(index(instruction, ChooseAllCells_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllCells(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseAllCells(modflow.GWF)
                case (iSWF)
                    call ChooseAllCells(modflow.SWF)
                case (iCLN)
                    call ChooseAllCells(modflow.CLN)
                end select
                
             else if(index(instruction, ChooseCellsByLayer_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllCells(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseCellsByLayer(FnumMUT,modflow.GWF)
                case (iSWF)
                    call ChooseCellsByLayer(FnumMUT,modflow.SWF)
                case (iCLN)
                    call ChooseCellsByLayer(FnumMUT,modflow.CLN)
                end select

             
             else if(index(instruction, ChooseCellAtXYZ_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllCells(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseCellAtXYZ(FnumMUT,modflow.GWF)
                case (iSWF)
                    call ChooseCellAtXYZ(FnumMUT,modflow.SWF)
                case (iCLN)
                    call ChooseCellAtXYZ(FnumMUT,modflow.CLN)
                end select
                
             else if(index(instruction, ChooseCellbyXYZ_LayerRange_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllCells(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.GWF)
                case (iSWF)
                    call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.SWF)
                case (iCLN)
                    call ChooseCellbyXYZ_LayerRange(FnumMUT,modflow.CLN)
                end select
                
            
            else if(index(instruction, ClearAllCells_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ClearAllCells(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ClearAllCells(modflow.GWF)
                case (iSWF)
                    call ClearAllCells(modflow.SWF)
                case (iCLN)
                    call ClearAllCells(modflow.CLN)
                end select

            
            else if(index(instruction, ChooseGBNodes_CMD)  /= 0) then
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
            
            else if(index(instruction, ChooseCellsFromGBElements_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    call ChooseCellsFromGBElementsTemplate(FnumMUT,TMPLT)
                case (iGWF)
                    call ChooseCellsFromGBElements(FnumMUT,modflow.GWF,TMPLT)
                case (iSWF)
                    call ChooseCellsFromGBElements(FnumMUT,modflow.SWF,TMPLT)
                case (iCLN)
                    call ChooseCellsFromGBElements(FnumMUT,modflow.CLN,TMPLT)
                end select

            else if(index(instruction, ChooseCellsFromGBNodes_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    call ChooseCellsFromGBNodesTemplate(FnumMUT,TMPLT)
                case (iGWF)
                    call ChooseCellsFromGBNodes(FnumMUT,modflow.GWF,TMPLT)
                case (iSWF)
                    call ChooseCellsFromGBNodes(FnumMUT,modflow.SWF,TMPLT)
                case (iCLN)
                    call ChooseCellsFromGBNodes(FnumMUT,modflow.CLN,TMPLT)
                end select
                
            else if(index(instruction, ChooseCellsFromFile_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    !call ChooseCellsFromFileTemplate(FnumMUT,TMPLT)
                    ! this will be done later
                case (iGWF)
                    call ChooseCellsFromFile(FnumMUT,modflow.GWF)
                case (iSWF)
                    call ChooseCellsFromFile(FnumMUT,modflow.SWF)
                case (iCLN)
                    call ChooseCellsFromFile(FnumMUT,modflow.CLN)
                end select
            
            else if(index(instruction, ChooseCellsByChosenZones_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    !call ChooseCellsFromFileTemplate(FnumMUT,TMPLT)
                    ! this will be done later
                case (iGWF)
                    call ChooseCellsByChosenZones(modflow.GWF)
                case (iSWF)
                    call ChooseCellsByChosenZones(modflow.SWF)
                case (iCLN)
                    call ChooseCellsByChosenZones(modflow.CLN)
                end select
                
             else if(index(instruction, ChooseAllZones_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseAllZones(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseAllZones(modflow.GWF)
                case (iSWF)
                    call ChooseAllZones(modflow.SWF)
                case (iCLN)
                    call ChooseAllZones(modflow.CLN)
                end select
             
             else if(index(instruction, ClearAllZones_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ClearAllZones(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ClearAllZones(modflow.GWF)
                case (iSWF)
                    call ClearAllZones(modflow.SWF)
                case (iCLN)
                    call ClearAllZones(modflow.CLN)
                end select

             else if(index(instruction, ChooseZoneNumber_CMD)  /= 0) then
                select case(ActiveDomain)
                !case (iTMPLT)
                !    call ChooseZoneNumber(FnumMUT,modflow,TMPLT)
                case (iGWF)
                    call ChooseZoneNumber(FnumMUT,modflow.GWF)
                case (iSWF)
                    call ChooseZoneNumber(FnumMUT,modflow.SWF)
                case (iCLN)
                    call ChooseZoneNumber(FnumMUT,modflow.CLN)
                end select
                
            else if(index(instruction, NewZone_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    !call ChooseCellsFromFileTemplate(FnumMUT,TMPLT)
                    ! this will be done later
                case (iGWF)
                    call NewZoneFromChosenCells(modflow.GWF)
                case (iSWF)
                    call NewZoneFromChosenCells(modflow.SWF)
                case (iCLN)
                    call NewZoneFromChosenCells(modflow.CLN)
                end select

            else if(index(instruction, FlagChosenNodesAsOuterBoundary_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    call FlagChosenNodesAsOuterBoundaryTMPLT(TMPLT)
                case (iGWF)
                    call FlagChosenNodesAsOuterBoundary(modflow.GWF)
                case (iSWF)
                    call FlagChosenNodesAsOuterBoundary(modflow.SWF)
                case (iCLN)
                    call FlagChosenNodesAsOuterBoundary(modflow.CLN)
                end select
            
            else if(index(instruction, FlagChosenCellInactive_CMD)  /= 0) then
                select case(ActiveDomain)
                case (iTMPLT)
                    call FlagChosenCellsInactiveTMPLT(TMPLT)
                case (iGWF)
                    call FlagChosenCellsInactive(modflow.GWF)
                case (iSWF)
                    call FlagChosenCellsInactive(modflow.SWF)
                case (iCLN)
                    call FlagChosenCellsInactive(modflow.CLN)
                end select
                
            ! GWF properties assignment
            else if(index(instruction, AssignMaterialtoGWF_CMD)  /= 0) then
                call AssignMaterialtoGWF(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignKhtoGWF_CMD)  /= 0) then
                call AssignKhtoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignKvtoGWF_CMD)  /= 0) then
                call AssignKvtoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignSstoGWF_CMD)  /= 0) then
                call AssignSstoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignSytoGWF_CMD)  /= 0) then
                call AssignSytoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignAlphatoGWF_CMD)  /= 0) then
                call AssignAlphatoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignBetatoGWF_CMD)  /= 0) then
                call AssignBetatoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignSrtoGWF_CMD)  /= 0) then
                call AssignSrtoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignBrookstoGWF_CMD)  /= 0) then
                call AssignBrookstoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, AssignStartingheadtoGWF_CMD)  /= 0) then
                call AssignStartingHeadtoDomain(FnumMUT,modflow.GWF)
            else if(index(instruction, InitialHeadFunctionOfZtoGWF_CMD)  /= 0) then
                call InitialHeadFunctionOfZtoGWF(FnumMUT,modflow.GWF)

            ! CLN properties assignment
            else if(index(instruction, AssignMaterialtoCLN_CMD)  /= 0) then
                call AssignMaterialtoCLN(FnumMUT,modflow.CLN)
            else if(index(instruction, AssignStartingDepthtoCLN_CMD)  /= 0) then
                call AssignStartingDepthtoDomain(FnumMUT,modflow.CLN)
                
            ! SWF properties assignment
            else if(index(instruction, AssignMaterialtoSWF_CMD)  /= 0) then
                call AssignMaterialtoSWF(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignSgcltoSWF_CMD)  /= 0) then
                call AssignSgcltoDomain(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignStartingDepthtoSWF_CMD)  /= 0) then
                call AssignStartingDepthtoDomain(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignManningtoSWF_CMD)  /= 0) then
                call AssignManningtoSWF(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignDepressiontoSWF_CMD)  /= 0) then
                call AssignDepressiontoSWF(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignObstructiontoSWF_CMD)  /= 0) then
                call AssignObstructiontoSWF(FnumMUT,modflow.SWF)
            else if(index(instruction, AssignDepthForSmoothingtoSWF_CMD)  /= 0) then
                call AssignDepthForSmoothingtoSWF(FnumMUT,modflow.SWF)

            ! GWF boundary contitions
            else if(index(instruction, AssignCHDtoGWF_CMD)  /= 0) then
                call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.GWF)
            else if(index(instruction, AssignDRNtoGWF_CMD)  /= 0) then
                call AssignDRNtoDomain(FnumMUT,Modflow,Modflow.GWF)
            else if(index(instruction, AssignRCHtoGWF_CMD)  /= 0) then
                call AssignRCHtoDomain(FnumMUT,Modflow,Modflow.GWF)
            else if(index(instruction, AssignWELtoGWF_CMD)  /= 0) then
                call AssignWELtoDomain(FnumMUT,Modflow,Modflow.GWF)

            ! SWF boundary contitions
            else if(index(instruction, AssignCHDtoSWF_CMD)  /= 0) then
                call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.SWF)
            else if(index(instruction, AssignRCHtoSWF_CMD)  /= 0) then
                call AssignRCHtoDomain(FnumMUT,Modflow,Modflow.SWF)
            else if(index(instruction, AssignWELtoSWF_CMD)  /= 0) then
                call AssignWELtoDomain(FnumMUT,Modflow,Modflow.SWF)
            else if(index(instruction, AssignCriticalDepthtoSWF_CMD)  /= 0) then
                call AssignCriticalDepthtoDomain(Modflow,Modflow.SWF)
            else if(index(instruction, AssignCriticalDepthtoCellsSide1_CMD)  /= 0) then
                call AssignCriticalDepthtoCellsSide1(Modflow,Modflow.SWF)
            
            ! CLN boundary contitions
            else if(index(instruction, AssignCHDtoCLN_CMD)  /= 0) then
                call AssignCHDtoDomain(FnumMUT,Modflow,Modflow.CLN)
            else if(index(instruction, AssignWELtoCLN_CMD)  /= 0) then
                call AssignWELtoDomain(FnumMUT,Modflow,Modflow.CLN)
            
            
            else if(index(instruction, GenOCFile_CMD)  /= 0) then
                call GenOCFile(FNumMUT,Modflow)
                
            else if(index(instruction, StressPeriod_CMD)  /= 0) then
                call StressPeriod(FNumMUT,Modflow)
                
            else
                call ErrMsg('MUSG?:'//instruction)
            end if
        end do

        10 continue

    end subroutine BuildModflowUSG

    !----------------------------------------------------------------------
    subroutine BuildModflowCLNDomain(Modflow,TMPLT,TMPLT_CLN)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_CLN

        integer :: i, j

        Modflow.CLN.name='CLN'
                
        if(Modflow.NodalControlVolume) then
            Modflow.CLN.nCells=TMPLT_CLN.nNodes
        else
            Modflow.CLN.nCells=TMPLT_CLN.nElements
        end if            
        
        Modflow.CLN.nNodesPerCell=TMPLT_CLN.nNodesPerElement
        Modflow.CLN.nNodes=TMPLT_CLN.nNodes  ! Used when choosing gb nodes
        Modflow.CLN.nElements=TMPLT_CLN.nElements  ! Used when choosing gb nodes

        ! Modflow CLN cell coordinate 
        allocate(Modflow.CLN.xCell(Modflow.CLN.nCells),Modflow.CLN.yCell(Modflow.CLN.nCells),Modflow.CLN.zCell(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' Cell coordinate arrays')
        if(Modflow.NodalControlVolume) then
            do i=1,Modflow.CLN.nCells
                Modflow.CLN.xCell(i)=TMPLT_CLN.x(i)
                Modflow.CLN.yCell(i)=TMPLT_CLN.y(i)
                Modflow.CLN.zCell(i)=TMPLT_CLN.z(i)
            end do
        else
            do i=1,Modflow.CLN.nCells
                Modflow.CLN.xCell(i)=TMPLT.xElement(i)
                Modflow.CLN.yCell(i)=TMPLT.yElement(i)
 
                ! zc from centroid of the iNode array coordinates
                zc=0.0
                do j=1,Modflow.CLN.nNodesPerCell
                    zc=zc+TMPLT_CLN.z(TMPLT_CLN.iNode(j,i))
                end do
                Modflow.CLN.zCell(i)=zc/Modflow.CLN.nNodesPerCell
            end do
        end if
        
        ! Cell node list
        allocate(Modflow.CLN.iNode(Modflow.CLN.nNodesPerCell,Modflow.CLN.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' Cell node list array')
        Modflow.CLN.iNode(:,:) = TMPLT_CLN.iNode(:,:)
       
        ! Cell properties lengths
        allocate(Modflow.CLN.Length(Modflow.CLN.nElements), &
            Modflow.CLN.LowestElevation(Modflow.CLN.nElements), &
            Modflow.CLN.SlopeAngle(Modflow.CLN.nElements), &
            stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' CLN Cell Length array')
        Modflow.CLN.Length(:) = TMPLT_CLN.Length(:)
        Modflow.CLN.LowestElevation(:) = TMPLT_CLN.LowestElevation(:)
        Modflow.CLN.SlopeAngle(:) = TMPLT_CLN.SlopeAngle(:)
               
        ! Modflow CLN cell layer number
        Modflow.CLN.nLayers=TMPLT_CLN.nLayers
        allocate(Modflow.CLN.iLayer(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' Element layer number array')
        do i=1,Modflow.CLN.nCells
            Modflow.CLN.iLayer(i) = 1
        end do


        ! Cell zone number
        Modflow.CLN.nZones=TMPLT_CLN.nZones
        allocate(Modflow.CLN.iZone(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' iZone array')
        !If(Modflow.NodalControlVolume) then
        !    ! Must account for element-based zone numbers in TMPLT_CLN
        !    do i=1,TMPLT_CLN.nElements
        !        do j=1,TMPLT_CLN.nNodesPerElement
        !            Modflow.CLN.iZone(TMPLT_CLN.iNode(j,i))=TMPLT_CLN.iZone(i)
        !        end do
        !    end do
        !else
            do i=1,Modflow.CLN.nCells
                Modflow.CLN.iZone(i)=TMPLT_CLN.iZone(i)
            end do
        !end if
        
        ! Cell Geometry: ia, ja arrays
        !if(Modflow.NodalControlVolume) then
        !    call NodeCentredCLNCellGeometry(Modflow, TMPLT_CLN,TMPLT)
        !else
            call MeshCentredCLNCellGeometry(Modflow, TMPLT_CLN,TMPLT)  ! Only mesh centred for now
        !end if
        
        ! Modflow CLN material properties (cell-based)
        allocate(modflow.CLN.Sgcl(modflow.CLN.nCells),modflow.CLN.CriticalDepthLength(modflow.CLN.nCells),Modflow.CLN.StartingHeads(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,'CLN cell material property arrays')    
        modflow.CLN.Sgcl(:)=0.001
        modflow.CLN.CriticalDepthLength(:)=0.d0
        Modflow.CLN.StartingHeads(:)=-999.d0
        
        ! Modflow CLN material properties (zone-based)
       allocate(Modflow.CLN.Geometry(Modflow.CLN.nZones), &                    
                Modflow.CLN.Direction(Modflow.CLN.nZones), &    
                Modflow.CLN.CircularRadius(Modflow.CLN.nZones), &  
                Modflow.CLN.RectangularWidth(Modflow.CLN.nZones), &  
                Modflow.CLN.RectangularHeight(Modflow.CLN.nZones), &  
                Modflow.CLN.LongitudinalK(Modflow.CLN.nZones), &  
                Modflow.CLN.FlowTreatment(Modflow.CLN.nZones), &  
            stat=ialloc)
        call AllocChk(ialloc,'CLN zoned material property arrays') 
        Modflow.CLN.Geometry(:)=-999
        Modflow.CLN.Direction(:)=-999
        Modflow.CLN.CircularRadius(:)=-999.d0
        Modflow.CLN.RectangularWidth(:)=-999.d0
        Modflow.CLN.RectangularHeight(:)=-999.d0
        Modflow.CLN.LongitudinalK(:)=-999.d0
        Modflow.CLN.FlowTreatment(:)=-999

        
        allocate(Modflow.CLN.Cell_Is(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.CLN.name)//' Cell_Is array')            
        Modflow.CLN.Cell_Is(:)=0
   
    end subroutine BuildModflowCLNDomain

    !----------------------------------------------------------------------
    subroutine BuildModflowGWFDomain(Modflow,TMPLT,TMPLT_GWF)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_GWF

        integer :: i, j, k

        ! For modflow cell connection and area calculations
        integer :: j1, j2, icell, iNode
        real(dr) :: TriangleArea
        
        integer :: iElement, iLay
        
        integer :: iConn, iNbor
        
        Modflow.GWF.name='GWF'

        if(Modflow.NodalControlVolume) then
            Modflow.GWF.nCells=TMPLT.nNodes*(TMPLT_GWF.nLayers+1)
            Modflow.GWF.nLayers=TMPLT_GWF.nLayers+1

        else
            Modflow.GWF.nCells=TMPLT_GWF.nElements
            Modflow.GWF.nLayers=TMPLT_GWF.nLayers
        end if            
        
        Modflow.GWF.nNodes=TMPLT_GWF.nNodes  ! Used when choosing gb nodes
        Modflow.GWF.nElements=TMPLT_GWF.nElements
        Modflow.GWF.nNodesPerCell=TMPLT_GWF.nNodesPerElement

        ! Modflow GWF cell coordinate 
        allocate(Modflow.GWF.xCell(Modflow.GWF.nCells),Modflow.GWF.yCell(Modflow.GWF.nCells),Modflow.GWF.zCell(Modflow.GWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.GWF.name)//' Cell coordinate arrays')
        if(Modflow.NodalControlVolume) then
            allocate(Modflow.GWF.Top(Modflow.GWF.nCells),Modflow.GWF.Bottom(Modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,trim(Modflow.GWF.name)//' top, bottom arrays')
            Modflow.GWF.Top(:)=0.0d0
            Modflow.GWF.Bottom(:)=0.0d0

            icell=0
            do j=1,TMPLT_GWF.nLayers+1
                do i=1,TMPLT.nNodes
                    icell=icell+1
                    iNode=i+(j-1)*TMPLT.nNodes
                    Modflow.GWF.xCell(iCell)=TMPLT_GWF.x(iNode) 
                    Modflow.GWF.yCell(iCell)=TMPLT_GWF.y(iNode) 
                    Modflow.GWF.zCell(icell)=TMPLT_GWF.z(iNode)
                    
                    if(j==1) then
                        Modflow.GWF.Top(icell)=TMPLT_GWF.z(iNode) 
                        Modflow.GWF.Bottom(icell)=(TMPLT_GWF.z(iNode)+TMPLT_GWF.z(iNode+TMPLT.nNodes))/2.0d0
                    else if(j==TMPLT_GWF.nLayers+1) then
                        Modflow.GWF.Top(icell)=(TMPLT_GWF.z(iNode)+TMPLT_GWF.z(iNode-TMPLT.nNodes))/2.0d0
                        Modflow.GWF.Bottom(icell)=TMPLT_GWF.z(iNode) 
                    else
                        Modflow.GWF.Top(icell)=(TMPLT_GWF.z(iNode)+TMPLT_GWF.z(iNode-TMPLT.nNodes))/2.0d0
                        Modflow.GWF.Bottom(icell)=(TMPLT_GWF.z(iNode)+TMPLT_GWF.z(iNode+TMPLT.nNodes))/2.0d0
                    end if
                end do
            end do
        else
            do i=1,Modflow.GWF.nCells
                iElement = myMOD(i,TMPLT.nElements)
                Modflow.GWF.xCell(i)=TMPLT.xElement(iElement)
                Modflow.GWF.yCell(i)=TMPLT.yElement(iElement)
                ! zc from centroid of the iNode array coordinates
                zc=0.0
                do j=1,Modflow.GWF.nNodesPerCell
                    zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
                end do
                Modflow.GWF.zCell(i)=zc/TMPLT_GWF.nNodesPerElement
            end do
        end if

        ! Element node list
        allocate(Modflow.GWF.iNode(Modflow.GWF.nNodesPerCell,Modflow.GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.GWF.name)//' Cell node list array')
        Modflow.GWF.iNode(:,:) = TMPLT_GWF.iNode(:,:)

        ! Element top and bottom
        if(.not. Modflow.NodalControlVolume) then
            allocate(Modflow.GWF.Top(Modflow.GWF.nCells),Modflow.GWF.Bottom(Modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,trim(Modflow.GWF.name)//' top, bottom arrays')
            Modflow.GWF.Top(:)=0.0d0
            Modflow.GWF.Bottom(:)=0.0d0
            do i=1,TMPLT_GWF.nElements
                if(TMPLT_GWF.nNodesPerElement == 6) then
                    ! zc from centroid of the iNode array z-coordinates for top face
                    zc=0.0
                    do j=4,6
                        zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
                    end do
                    Modflow.GWF.Top(i)=zc/3
        
                    ! zc from centroid of the iNode array z-coordinates for bottom face
                    zc=0.0
                    do j=1,3
                        zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
                    end do
                    Modflow.GWF.Bottom(i)=zc/3
            
                else if(TMPLT_GWF.nNodesPerElement == 8) then
                    ! zc from centroid of the iNode array z-coordinates for top face
                    zc=0.0
                    do j=5,8
                        zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
                    end do
                    Modflow.GWF.Top(i)=zc/4
        
                    ! zc from centroid of the iNode array z-coordinates for top face
                    zc=0.0
                    do j=1,4
                        zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
                    end do
                    Modflow.GWF.Bottom(i)=zc/4
                end if
            end do
        end if

            
        ! Modflow GWF cell layer number
        allocate(Modflow.GWF.iLayer(Modflow.GWF.nCells),stat=ialloc)
        if(Modflow.NodalControlVolume) then
            call AllocChk(ialloc,trim(Modflow.GWF.name)//' cell layer number array')
            do i=1,Modflow.GWF.nCells
                if(myMod(i,TMPLT.nNodes) == TMPLT.nNodes) then
                    iLay = i/(TMPLT.nNodes)
                else 
                    iLay = i/(TMPLT.nNodes)+1
                end if
                   
                Modflow.GWF.iLayer(i) = iLay
            end do
        else
            call AllocChk(ialloc,trim(Modflow.GWF.name)//' cell layer number array')
            do i=1,Modflow.GWF.nCells
                Modflow.GWF.iLayer(i) = TMPLT_GWF.iLayer(i) 
            end do
        end if
        
        ! Layer has quasi-3D confining bed below if non-zero
        allocate(Modflow.GWF.Laybcd(Modflow.GWF.nLayers),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.GWF.name)//'.Laybcd array')
        Modflow.GWF.Laybcd(:)=0
        
        ! Cell zone number
        Modflow.GWF.nZones=TMPLT_GWF.nZones
        allocate(Modflow.GWF.iZone(Modflow.GWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.GWF.name)//' iZone arrays')
        If(Modflow.NodalControlVolume) then
            CellLoop: do k=1,Modflow.GWF.nCells
                do i=1,TMPLT_GWF.nElements
                    do j=1,TMPLT_GWF.nNodesPerElement
                        if(TMPLT_GWF.iNode(j,i) == k) then
                            Modflow.GWF.iZone(k)=TMPLT_GWF.iZone(i)
                            cycle CellLoop
                        end if
                    end do
                end do
            end do CellLoop
        else
            do i=1,Modflow.GWF.nCells
                Modflow.GWF.iZone(i)=TMPLT_GWF.iZone(i)
            end do
        end if


        ! GWF Cell connection data
        If(Modflow.NodalControlVolume) then
            ! calculate node-centred cell area based on TMPLT.CellArea
            ! do top layer first
            allocate(Modflow.GWF.CellArea(Modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'GWF cell horizontal area arrays')
            Modflow.GWF.CellArea(:)=0.0d0
            do i=1,TMPLT.nElements
                do j=1,TMPLT.nNodesPerElement
                    j1=TMPLT.iNode(j,i)
                    if(j < TMPLT.nNodesPerElement) then
                        j2=TMPLT.iNode(j+1,i)
                    else
                        j2=TMPLT.iNode(1,i)
                    end if
                
                
                    call area_triangle(TMPLT.x(j1),TMPLT.y(j1),0.0d0, &
                            &          TMPLT.xSide(j,i),TMPLT.ySide(j,i),0.0d0, &
                            &          TMPLT.xCircle(i),TMPLT.yCircle(i),0.0d0, &
                            &          TriangleArea)
                    Modflow.GWF.CellArea(j1)=Modflow.GWF.CellArea(j1)+TriangleArea
        
                    call area_triangle(TMPLT.x(j2),TMPLT.y(j2),0.0d0, &
                            &          TMPLT.xCircle(i),TMPLT.yCircle(i),0.0d0, &
                            &          TMPLT.xSide(j,i),TMPLT.ySide(j,i),0.0d0, &
                            &          TriangleArea)
                    Modflow.GWF.CellArea(j2)=Modflow.GWF.CellArea(j2)+TriangleArea
                
                end do
            end do
                
            ! copy through all layers
            do i=1,TMPLT.nNodes
                do j=2,Modflow.GWF.nLayers
                    iCell = (j-1)*TMPLT.nNodes+i
                    Modflow.GWF.CellArea(icell)=Modflow.GWF.CellArea(i)
                end do
            end do
        
                
        else
            ! use existing TMPLT.CellArea (e.g. area of triangle)  
            allocate(Modflow.GWF.CellArea(Modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'GWF cell horizontal area arrays')
            do i=1,Modflow.GWF.nCells
                iCell = myMOD(i,TMPLT.nElements)
                Modflow.GWF.CellArea(i)=TMPLT.ElementArea(iCell)
            end do
        end if
            
        
        ! Cell connection length and perpendicular area and ia/ja arrays
        If(Modflow.NodalControlVolume) then
            allocate(Modflow.GWF.ConnectionLength(TMPLT_GWF.njag),Modflow.GWF.PerpendicularArea(TMPLT_GWF.njag),stat=ialloc)
            call AllocChk(ialloc,'GWF Cell connection length, perpendicular area array')
            Modflow.GWF.ConnectionLength(:)=0.0d0
            Modflow.GWF.PerpendicularArea(:)=0.0d0
            
            Modflow.GWF.njag=TMPLT_GWF.njag
            
            allocate(Modflow.GWF.ia(Modflow.GWF.nCells),Modflow.GWF.ja(TMPLT_GWF.njag),stat=ialloc)
            call AllocChk(ialloc,'GWF ia, ja arrays')
            Modflow.GWF.ia(:)=TMPLT_GWF.ia
            Modflow.GWF.ja(:)=TMPLT_GWF.ja
            
            Modflow.GWF.nodelay=TMPLT.nNodes
            iConn=0
            iNbor=0
            do i=1,Modflow.GWF.nCells
                iCell = myMOD(i,Modflow.GWF.nodelay)
                iConn=iConn+1
                    
                do j=2,modflow.GWF.ia(i)
                    iConn=iConn+1
                    inBor=iNbor+1
                    if(myMOD(i,modflow.GWF.nodelay) == myMOD(modflow.GWF.ja(iconn),modflow.GWF.nodelay)) then ! GWF neighbour in same column
                        modflow.GWF.ConnectionLength(iConn)=(Modflow.GWF.Top(i)-Modflow.GWF.Bottom(i))/2.0d0
                        modflow.GWF.PerpendicularArea(iConn)=Modflow.GWF.CellArea(iCell)
                    else  ! SWF or GWF neighbour in adjacent column
                        modflow.GWF.ConnectionLength(iConn)=TMPLT_GWF.ConnectionLength(iCoNN)
                        modflow.GWF.PerpendicularArea(iConn)=TMPLT_GWF.PerpendicularArea(iConn)*(Modflow.GWF.Top(i)-Modflow.GWF.Bottom(i))
                    end if
                end do
            end do

            
                
        else
            allocate(Modflow.GWF.ConnectionLength(TMPLT_GWF.njag),Modflow.GWF.PerpendicularArea(TMPLT_GWF.njag),stat=ialloc)
            call AllocChk(ialloc,'GWF Cell connection length, perpendicular area array')
            Modflow.GWF.ConnectionLength(:)=0.0d0
            Modflow.GWF.PerpendicularArea(:)=0.0d0
        
            Modflow.GWF.njag=TMPLT_GWF.njag
        
            allocate(Modflow.GWF.ia(TMPLT_GWF.nElements),Modflow.GWF.ja(TMPLT_GWF.njag),stat=ialloc)
            call AllocChk(ialloc,'GWF ia, ja arrays')
            Modflow.GWF.ia(:)=TMPLT_GWF.ia
            Modflow.GWF.ja(:)=TMPLT_GWF.ja
        
            Modflow.GWF.nodelay=TMPLT.nElements
            iConn=0
            iNbor=0
            do i=1,TMPLT_GWF.nElements
                iCell = myMOD(i,TMPLT.nElements)
                iConn=iConn+1
                    
                do j=2,modflow.GWF.ia(i)
                    iConn=iConn+1
                    inBor=iNbor+1
                    if(myMOD(i,modflow.GWF.nodelay) == myMOD(modflow.GWF.ja(iconn),modflow.GWF.nodelay)) then ! GWF neighbour in same column
                        modflow.GWF.ConnectionLength(iConn)=(Modflow.GWF.Top(i)-Modflow.GWF.Bottom(i))/2.0d0
                        modflow.GWF.PerpendicularArea(iConn)=TMPLT.ElementArea(iCell)
                    else  ! SWF or GWF neighbour in adjacent column
                        if(TMPLT.nNodesPerElement == 3) then
                            select case (TMPLT_GWF.face(iNbor))
                            case ( 1 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(3,iCell)   
                            case ( 2 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(2,iCell)   
                            case ( 3 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(1,iCell)   
                            end select
                            modflow.GWF.ConnectionLength(iConn)=TMPLT.rCircle(iCell)
                            modflow.GWF.PerpendicularArea(iConn)=modflow.GWF.PerpendicularArea(iConn)*(Modflow.GWF.Top(i)-Modflow.GWF.Bottom(i))
                        else if(TMPLT.nNodesPerElement == 4) then
                            select case (TMPLT_GWF.face(iNbor))
                            case ( 1 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(1,iCell)   
                                modflow.GWF.ConnectionLength(iConn)=TMPLT.SideLength(1,iCell)/2.0d0
                            case ( 2 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(2,iCell)   
                                modflow.GWF.ConnectionLength(iConn)=TMPLT.SideLength(2,iCell)/2.0d0
                            case ( 3 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(3,iCell)   
                                modflow.GWF.ConnectionLength(iConn)=TMPLT.SideLength(3,iCell)/2.0d0
                            case ( 4 )
                                modflow.GWF.PerpendicularArea(iConn)=TMPLT.SideLength(4,iCell)   
                                modflow.GWF.ConnectionLength(iConn)=TMPLT.SideLength(4,iCell)/2.0d0
                            end select
                            modflow.GWF.PerpendicularArea(iConn)=modflow.GWF.PerpendicularArea(iConn)*(Modflow.GWF.Top(i)-Modflow.GWF.Bottom(i))
                        end if
                    end if
                end do
            end do
            
        end if
        
        ! Modflow GWF material properties (cell-based)
        allocate(Modflow.GWF.Kh(Modflow.GWF.nCells),Modflow.GWF.Kv(Modflow.GWF.nCells), &
            Modflow.GWF.Ss(Modflow.GWF.nCells),Modflow.GWF.Sy(Modflow.GWF.nCells),&
            Modflow.GWF.Alpha(Modflow.GWF.nCells),Modflow.GWF.Beta(Modflow.GWF.nCells),Modflow.GWF.Sr(Modflow.GWF.nCells), &
            Modflow.GWF.Brooks(Modflow.GWF.nCells),Modflow.GWF.StartingHeads(Modflow.GWF.nCells), &
            stat=ialloc)
        call AllocChk(ialloc,'GWF cell material property arrays')            
        Modflow.GWF.Kh(:)=-999.d0
        Modflow.GWF.Kh(:)=-999.d0
        Modflow.GWF.Ss(:)=-999.d0
        Modflow.GWF.Sy(:)=-999.d0
        Modflow.GWF.Alpha(:)=-999.d0
        Modflow.GWF.Beta(:)=-999.d0
        Modflow.GWF.Sr(:)=-999.d0
        Modflow.GWF.Brooks(:)=-999.d0
        Modflow.GWF.StartingHeads(:)=-999.d0

        allocate(Modflow.GWF.Cell_Is(Modflow.GWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.GWF.name)//' Cell_Is array')            
        Modflow.GWF.Cell_Is(:)=0
    
    end subroutine BuildModflowGWFDomain
    
    !----------------------------------------------------------------------
    subroutine BuildModflowSWFDomain(Modflow,TMPLT,TMPLT_SWF)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_SWF

        integer :: i, j

        Modflow.SWF.name='SWF'
                
        if(Modflow.NodalControlVolume) then
            Modflow.SWF.nCells=TMPLT_SWF.nNodes
            Modflow.SWF.nodelay=TMPLT_SWF.nNodes

        else
            Modflow.SWF.nCells=TMPLT_SWF.nElements
            Modflow.SWF.nodelay=TMPLT_SWF.nElements
        end if            
        
        Modflow.SWF.nNodesPerCell=TMPLT_SWF.nNodesPerElement
        Modflow.SWF.nNodes=TMPLT_SWF.nNodes  ! Used when choosing gb nodes, flagging outer boundary nodes
        Modflow.SWF.nElements=TMPLT_SWF.nElements  ! Used when choosing gb elements

        ! Modflow SWF cell coordinate 
        allocate(Modflow.SWF.xCell(Modflow.SWF.nCells),Modflow.SWF.yCell(Modflow.SWF.nCells),Modflow.SWF.zCell(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Cell coordinate arrays')
        if(Modflow.NodalControlVolume) then
            do i=1,Modflow.SWF.nCells
                Modflow.SWF.xCell(i)=TMPLT_SWF.x(i)
                Modflow.SWF.yCell(i)=TMPLT_SWF.y(i)
                Modflow.SWF.zCell(i)=TMPLT_SWF.z(i)
            end do
        else
            do i=1,Modflow.SWF.nCells
                Modflow.SWF.xCell(i)=TMPLT.xElement(i)
                Modflow.SWF.yCell(i)=TMPLT.yElement(i)
 
                ! zc from centroid of the iNode array coordinates
                zc=0.0
                do j=1,Modflow.SWF.nNodesPerCell
                    zc=zc+TMPLT_SWF.z(TMPLT_SWF.iNode(j,i))
                end do
                Modflow.SWF.zCell(i)=zc/Modflow.SWF.nNodesPerCell
            end do
        end if
        
        ! Modflow SWF node coordinates 
        allocate(Modflow.SWF.x(Modflow.SWF.nNodes),Modflow.SWF.y(Modflow.SWF.nNodes),Modflow.SWF.z(Modflow.SWF.nNodes),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Node coordinate arrays')
        do i=1,Modflow.SWF.nNodes
            Modflow.SWF.x(i)=TMPLT_SWF.x(i)
            Modflow.SWF.y(i)=TMPLT_SWF.y(i)
            Modflow.SWF.z(i)=TMPLT_SWF.z(i)
        end do

            ! Element node list
        allocate(Modflow.SWF.iNode(Modflow.SWF.nNodesPerCell,Modflow.SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Cell node list array')
        Modflow.SWF.iNode(:,:) = TMPLT_SWF.iNode(:,:)
       
        ! Element side lengths
        allocate(Modflow.SWF.SideLength(Modflow.SWF.nNodesPerCell,Modflow.SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Cell SideLength array')
        Modflow.SWF.SideLength(:,:) = TMPLT_SWF.SideLength(:,:)
               
        ! Modflow SWF cell layer number
        Modflow.SWF.nLayers=TMPLT_SWF.nLayers
        allocate(Modflow.SWF.iLayer(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Element layer number array')
        do i=1,Modflow.SWF.nCells
            Modflow.SWF.iLayer(i) = 1
        end do


        ! Cell zone number
        Modflow.SWF.nZones=TMPLT_SWF.nZones
        allocate(Modflow.SWF.iZone(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' iZone array')
        If(Modflow.NodalControlVolume) then
            ! Must account for element-based zone numbers in TMPLT_SWF
            do i=1,TMPLT_SWF.nElements
                do j=1,TMPLT_SWF.nNodesPerElement
                    Modflow.SWF.iZone(TMPLT_SWF.iNode(j,i))=TMPLT_SWF.iZone(i)
                end do
            end do
        else
            do i=1,Modflow.SWF.nCells
                Modflow.SWF.iZone(i)=TMPLT_SWF.iZone(i)
            end do
        end if
        
        ! Cell Geometry: ia, ja arrays
        if(Modflow.NodalControlVolume) then
            call NodeCentredSWFCellGeometry(Modflow, TMPLT_SWF,TMPLT)
        else
            call MeshCentredSWFCellGeometry(Modflow, TMPLT_SWF,TMPLT)
        end if
        
        ! Modflow SWF material properties (cell-based)
        allocate(modflow.SWF.Sgcl(modflow.SWF.nCells),modflow.SWF.CriticalDepthLength(modflow.SWF.nCells),Modflow.SWF.StartingHeads(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'SWF cell material property arrays')    
        modflow.SWF.Sgcl(:)=0.001
        modflow.SWF.CriticalDepthLength(:)=0.d0
        Modflow.SWF.StartingHeads(:)=-999.d0
        
        ! Modflow SWF material properties (zone-based)
       allocate(Modflow.SWF.Manning(Modflow.SWF.nZones), &                   
                Modflow.SWF.DepressionStorageHeight(Modflow.SWF.nZones), &   
                Modflow.SWF.ObstructionStorageHeight(Modflow.SWF.nZones), &  
                Modflow.SWF.H1DepthForSmoothing(Modflow.SWF.nZones), &       
                Modflow.SWF.H2DepthForSmoothing(Modflow.SWF.nZones), &       
            stat=ialloc)
        call AllocChk(ialloc,'SWF zoned material property arrays')            
        Modflow.SWF.Manning(:)=-999.d0
        Modflow.SWF.DepressionStorageHeight(:)=-999.d0
        Modflow.SWF.ObstructionStorageHeight(:)=-999.d0
        Modflow.SWF.H1DepthForSmoothing(:)=-999.d0
        Modflow.SWF.H2DepthForSmoothing(:)=-999.d0

        
        allocate(Modflow.SWF.Cell_Is(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,trim(Modflow.SWF.name)//' Cell_Is array')            
        Modflow.SWF.Cell_Is(:)=0
   
    end subroutine BuildModflowSWFDomain

    !----------------------------------------------------------------------
    subroutine ChooseAllZones(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount


        
        if(.not. allocated(domain.Zone_Is)) then 
            allocate(domain.Zone_Is(domain.nZones),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Zone_Is array')            
            domain.Zone_Is(:)=0
        end if
        
        ncount=0
        do i=1,domain.nZones
            call set(domain.Zone_Is(i),chosen)
            ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' zone numbers currently chosen: '
        call Msg(trim(TmpSTR))
        do i=1,domain.nZones
            if(bcheck(domain.Zone_Is(i),chosen)) then
                write(TmpSTR,'(a,i5)') TAB,i
                call Msg(trim(TmpSTR))
            endif
        end do

	    if(ncount == 0) call ErrMsg('No Zones chosen')
	    
    end subroutine ChooseAllZones
    !----------------------------------------------------------------------
    subroutine ChooseZoneNumber(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain

        integer :: i
        integer :: number
        
        if(.not. allocated(domain.Zone_Is)) then 
            allocate(domain.Zone_Is(domain.nZones),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Zone_Is array')            
            domain.Zone_Is(:)=0
        end if

		read(FNumMUT,*) number
        write(TmpSTR,'(a,i8)') TAB//'Adding zone number: ',number
		call Msg(trim(TmpSTR))
        
        if(number <= 0 .or. number > domain.nZones) then
            write(TmpSTR,'(a,i8)') 'Number must be between 1 and ',domain.nZones
            call Errmsg(trim(TmpSTR))
        end if

        call set(domain.Zone_Is(number),chosen)

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' zone numbers currently chosen: '
        call Msg(trim(TmpSTR))
        do i=1,domain.nZones
            if(bcheck(domain.Zone_Is(i),chosen)) then
                write(TmpSTR,'(a,i5)') TAB,i
                call Msg(trim(TmpSTR))
            endif
        end do
                
    
    end subroutine ChooseZoneNumber
    !----------------------------------------------------------------------
    subroutine ChooseAllNodes(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount


        
        if(.not. allocated(domain.Node_Is)) then 
            allocate(domain.Node_Is(domain.nNodes),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Node_Is array')            
            domain.Node_Is(:)=0
        end if
        
        ncount=0
        do i=1,domain.nNodes
            call set(domain.Node_Is(i),chosen)
            ncount=ncount+1
        end do

        write(ieco,*) 'Nodes chosen: ',ncount
	    if(ncount == 0) call ErrMsg('No nodes chosen')
	    
    end subroutine ChooseAllNodes

    !----------------------------------------------------------------------
    subroutine ChooseGBNodes(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain

        integer :: i, j
	    integer :: nLayer_bot, nLayer_top, ncount, iNode

        character(MAX_STR) :: FName
        character*80 :: dummy
        logical :: togon(domain.nNodes)

        
        if(.not. allocated(domain.Node_Is)) then 
            allocate(domain.Node_Is(domain.nNodes),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Node_Is array')            
            domain.Node_Is(:)=0
        end if

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,domain.nNodes)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
        if(domain.name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,Domain.nLayers)
            nLayer_top=min(nLayer_top,Domain.nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
		    call Msg(TAB//'From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
		    call Msg(TAB//'To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain.nNodes
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iNode=(j-1)*domain.nNodes+i
                        call set(Domain.Node_Is(iNode),chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
       
         else
            ncount=0
            do i=1,domain.nNodes
                if(togon(i)) then
                    call set(domain.Node_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount == 0) call ErrMsg('No nodes chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseGBNodes
    !----------------------------------------------------------------------
    subroutine ChooseGBNodesTemplate(FNumMUT,TMPLT) 
        implicit none

        integer :: FNumMUT
        type (TecplotDomain) TMPLT

        integer :: i
	    integer :: ncount

        character(MAX_STR) :: FName
        character*80 :: dummy
        logical :: togon(TMPLT.nNodes)

        
        if(.not. allocated(TMPLT.Node_Is)) then 
            allocate(TMPLT.Node_Is(TMPLT.nNodes),stat=ialloc)
            call AllocChk(ialloc,trim(TMPLT.name)//' Node_Is array')            
            TMPLT.Node_Is(:)=0
        end if

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT.nNodes)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
      !  if(TMPLT.name == 'GWF') then
      !
      !      read(FNumMUT,*) nLayer_top,nLayer_bot
      !  
      !      nLayer_bot=max(nLayer_bot,1)
      !      nLayer_bot=min(nLayer_bot,TMPLT.nLayers)
      !      nLayer_top=min(nLayer_top,TMPLT.nLayers)
      !      nLayer_top=max(nLayer_top,1)
      !  
      !      write(TmpSTR,'(i5)') nLayer_top
		    !call Msg(TAB//'From Layer: '//trim(TmpSTR))
      !      write(TmpSTR,'(i5)') nLayer_bot
		    !call Msg(TAB//'To Layer:   '//trim(TmpSTR))
      !
      !      ncount=0
      !      do i=1,TMPLT.nNodes
      !          if(togon(i)) then
      !              do j=nLayer_top,nLayer_bot
      !                  iNode=(j-1)*TMPLT.nNodes+i
      !                  call set(TMPLT.Node_Is(iNode),chosen)
      !                  ncount=ncount+1
      !              end do
      !          end if
      !      end do
      ! 
      !   else
            ncount=0
            do i=1,TMPLT.nNodes
                if(togon(i)) then
                    call set(TMPLT.Node_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        !end if

        write(TmpSTR,'(a,i10)') TAB//trim(TMPLT.name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount == 0) call ErrMsg('No nodes chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseGBNodesTemplate
    !----------------------------------------------------------------------
    subroutine ChooseAllCells(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount


        ncount=0
        do i=1,domain.nCells
            call set(domain.Cell_Is(i),chosen)
            ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount == 0) call ErrMsg('No Cells chosen')

    end subroutine ChooseAllCells
    !----------------------------------------------------------------------
    subroutine ChooseNodeAtXYZTemplate(FNumMut,Domain)
        implicit none
        
        integer :: FNumMUT
        type(TecplotDomain) Domain

	    integer :: i,iNode
	    real(dr) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) TAB//'Find node closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
	    do i=1,domain.nnodes
		    f1=sqrt((x1-domain.x(i))**2+((y1-domain.y(i)))**2+((z1-domain.z(i)))**2)
		    if(f1.lt.dist_min) then
			    inode=i
			    dist_min=f1
		    endif
	    end do
        call set(domain.Node_Is(iNode),chosen)
        
        write(tmpSTR,'(a14,3f17.5,a)') TAB//'Found x, y, z  ',domain.x(iNode),domain.y(iNode),domain.z(iNode),'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
		write(tmpSTR,'(a14,3f17.5,a)') TAB//'Delta x, y, z  ',domain.x(iNode)-x1,domain.y(iNode)-y1,domain.z(iNode)-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)

    end subroutine ChooseNodeAtXYZTemplate
    !----------------------------------------------------------------------
    subroutine ChooseNodeAtXYZ(FNumMut,Domain)
        implicit none
        
        integer :: FNumMUT
        type(ModflowDomain) Domain

	    integer :: i,iNode
	    real(dr) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) TAB//'Find node closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
	    do i=1,domain.nnodes
		    f1=sqrt((x1-domain.x(i))**2+((y1-domain.y(i)))**2+((z1-domain.z(i)))**2)
		    if(f1.lt.dist_min) then
			    inode=i
			    dist_min=f1
		    endif
	    end do
        call set(domain.Node_Is(iNode),chosen)
        
        write(tmpSTR,'(a14,3f17.5,a)') TAB//'Found x, y, z  ',domain.x(iNode),domain.y(iNode),domain.z(iNode),'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
		write(tmpSTR,'(a14,3f17.5,a)') TAB//'Delta x, y, z  ',domain.x(iNode)-x1,domain.y(iNode)-y1,domain.z(iNode)-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)

    end subroutine ChooseNodeAtXYZ
    !----------------------------------------------------------------------
    subroutine ChooseCellsByLayer(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain

        integer :: i, iLyr
	    integer :: ncount

        read(FNumMut,*) iLyr
        write(TMPStr,*) TAB//'Choose cells in layer: ',iLyr
        call Msg(TMPStr)

        ncount=0
        do i=1,domain.nCells
            if(domain.iLayer(i) == iLyr) then
                call set(domain.Cell_Is(i),chosen)
                ncount=ncount+1
            endif
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen in layer: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount == 0) call ErrMsg('No Cells chosen')

    end subroutine ChooseCellsByLayer
    !----------------------------------------------------------------------
    subroutine ChooseCellAtXYZ(FNumMut,Domain)
        implicit none
        
        integer :: FNumMUT
        type(ModflowDomain) Domain

	    integer :: i,iCell
	    real(dr) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) TAB//'Find cell closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
	    do i=1,domain.nCells
		    f1=sqrt((x1-domain.xCell(i))**2+((y1-domain.yCell(i)))**2+((z1-domain.zCell(i)))**2)
		    if(f1.lt.dist_min) then
			    iCell=i
			    dist_min=f1
		    endif
	    end do
        call set(domain.Cell_Is(iCell),chosen)
        
        write(tmpSTR,'(a14,3f17.5,a)') TAB//'Found x, y, z  ',domain.xCell(iCell),domain.yCell(iCell),domain.zCell(iCell),'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
		write(tmpSTR,'(a14,3f17.5,a)') TAB//'Delta x, y, z  ',domain.xCell(iCell)-x1,domain.yCell(iCell)-y1,domain.zCell(iCell)-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)

    end subroutine ChooseCellAtXYZ
    !----------------------------------------------------------------------
    subroutine ChooseCellbyXYZ_LayerRange(FNumMut,Domain)
        implicit none
        
        integer :: FNumMUT
        type(ModflowDomain) Domain

	    integer :: i,iCell
	    real(dr) :: x1,x2
	    real(dr) :: y1,y2
	    real(dr) :: z1,z2
	    integer :: ltop,lbot,ntemp,ielmin,ielmax

        call Msg('Find cells whose centroids are in the range defined by: ')
        read(FNumMut,*) x1,x2
        write(TMPStr,*) TAB//'X range: ',x1, x2
        call Msg(TMPStr)

        read(FNumMut,*) y1,y2
        write(TMPStr,*) TAB//'Y range: ',y1, y2
        call Msg(TMPStr)

        read(FNumMut,*) z1,z2
        write(TMPStr,*) TAB//'Z range: ',z1, z2
        call Msg(TMPStr)

        read(FNumMut,*) ltop,lbot
        write(TMPStr,*) TAB//'Layer range: ',ltop, lbot
        call Msg(TMPStr)

        if(ltop.gt.lbot) then
            ntemp=ltop
            ltop=lbot
            lbot=ntemp
        end if
        ielmin=(ltop-1)*domain.nodelay
        ielmax=lbot*domain.nodelay + 1

        x1=x1-small
        x2=x2+small
        y1=y1-small
        y2=y2+small
        z1=z1-small
        z2=z2+small

        ncount=0
        do i=1,domain.nCells
            if(domain.xCell(i).ge.x1 .and. domain.xCell(i).le.x2 .and. domain.yCell(i).ge.y1 .and. domain.yCell(i).le.y2 .and. domain.zCell(i).ge.z1 .and. domain.zCell(i).le.z2) then
                if(i .gt. ielmin .and. i .lt. ielmax) then
                    call set(domain.Cell_Is(i),chosen)
                    ncount=ncount+1
                end if
            end if
        end do
    
        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')
	    
    end subroutine ChooseCellbyXYZ_LayerRange

    !----------------------------------------------------------------------
    subroutine ChooseCellsFromFile(FNumMUT,domain) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount, iCell,status2

        character*80 fname
        logical togon(domain.nCells)

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose Cells from ascii file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown')
        togon(:)=.false.
        do
            read(itmp,*,iostat=status2) iCell
            if(status2/=0) exit
            togon(iCell)=.true.
        enddo
        
        ncount=0
        do i=1,domain.nCells
            if(togon(i)) then
                call set(Domain.Cell_Is(i),chosen)
                ncount=ncount+1
            end if
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseCellsFromFile

    !----------------------------------------------------------------------
    subroutine ChooseCellsByChosenZones(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i, j
	    integer :: ncount


       
        ncount=0
        do i=1,domain.nZones
            if(bcheck(domain.Zone_is(i),chosen)) then
                do j=1,domain.nCells
                    if(domain.iZone(j) == i) then
                        call set(Domain.Cell_Is(j),chosen)
                        ncount=ncount+1
                    end if
                end do
            end if
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')
	    


    end subroutine ChooseCellsByChosenZones

    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBElements(FNumMUT,domain,TMPLT) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        type (TecplotDomain) TMPLT

        integer :: i, j
	    integer :: nLayer_bot, nLayer_top, ncount, iCell

        character*80 fname
        character*80 dummy
        logical togon(domain.nCells)

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose Cells from gb chosen elements file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT.nElements)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
        if(domain.name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,Domain.nLayers)
            nLayer_top=min(nLayer_top,Domain.nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
		    call Msg(TAB//'From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
		    call Msg(TAB//'To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain.nCells
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iCell=(j-1)*TMPLT.nElements+i
                        call set(Domain.Cell_Is(iCell),chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,domain.nCells
                if(togon(i)) then
                    call set(Domain.Cell_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBElements
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBElementsTemplate(FNumMUT,TMPLT) 
        implicit none

        integer :: FNumMUT
        type (TecplotDomain) TMPLT

        integer :: i, j
	    integer :: nLayer_bot, nLayer_top, ncount, iElement

        character*80 fname
        character*80 dummy
        logical togon(TMPLT.nElements)

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose Elements from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT.nElements)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
        if(TMPLT.name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,TMPLT.nLayers)
            nLayer_top=min(nLayer_top,TMPLT.nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
		    call Msg(TAB//'From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
		    call Msg(TAB//'To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,TMPLT.nElements
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iElement=(j-1)*TMPLT.nElements+i
                        call set(TMPLT.Element_Is(iElement),chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,TMPLT.nElements
                if(togon(i)) then
                    call set(TMPLT.Element_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') TAB//trim(TMPLT.name)//' Elements chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Elements chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBElementsTemplate
   
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBNodes(FNumMUT,domain,TMPLT) 
        implicit none

        integer :: FNumMUT
        type (ModflowDomain) Domain
        type (TecplotDomain) TMPLT

        integer :: i, j
	    integer :: nLayer_bot, nLayer_top, ncount, iCell

        character*80 fname
        character*80 dummy
        logical togon(domain.nCells)

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose Cells from GB chosen nodes file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT.nNodes)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
        if(domain.name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,Domain.nLayers)
            nLayer_top=min(nLayer_top,Domain.nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
		    call Msg(TAB//'From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
		    call Msg(TAB//'To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain.nCells
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iCell=(j-1)*TMPLT.nNodes+i
                        call set(Domain.Cell_Is(iCell),chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,domain.nCells
                if(togon(i)) then
                    call set(Domain.Cell_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBNodes
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBNodesTemplate(FNumMUT,TMPLT) 
        implicit none

        integer :: FNumMUT
        type (TecplotDomain) TMPLT

        integer :: i, j
	    integer :: nLayer_bot, nLayer_top, ncount, iElement

        character*80 fname
        character*80 dummy
        logical togon(TMPLT.nNodes)

		read(FNumMUT,'(a)') fname
		call Msg(TAB//'Choose Nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT.nNodes)
        if(status /= 0) then
		    call ErrMsg('While reading: '//fname)
        end if
        
        if(TMPLT.name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,TMPLT.nLayers)
            nLayer_top=min(nLayer_top,TMPLT.nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
		    call Msg(TAB//'From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
		    call Msg(TAB//'To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,TMPLT.nNodes
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iElement=(j-1)*TMPLT.nNodes+i
                        call set(TMPLT.Element_Is(iElement),chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,TMPLT.nNodes
                if(togon(i)) then
                    call set(TMPLT.Element_Is(i),chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') TAB//trim(TMPLT.name)//' Nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Nodes chosen')
	    
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBNodesTemplate
    
    !----------------------------------------------------------------------
    subroutine ClearAllCells(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount


        do i=1,domain.nCells
            call clear(domain.Cell_Is(i),chosen)
        end do
        
        ncount=0
        do i=1,domain.nCells
            if(bcheck(domain.Cell_Is(i),chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount /= 0) call ErrMsg('Some Cells chosen')

    end subroutine ClearAllCells
    
    !----------------------------------------------------------------------
    subroutine ClearAllNodes(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount

        if(.not. allocated(domain.Node_Is)) then 
            allocate(domain.Node_Is(domain.nNodes),stat=ialloc)
            call AllocChk(ialloc,'Node_Is array')            
            domain.Node_Is(:)=0
        end if

        do i=1,domain.nNodes
            call clear(domain.Node_Is(i),chosen)
        end do
        
        ncount=0
        do i=1,domain.nNodes
            if(bcheck(domain.Node_Is(i),chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount /= 0) call ErrMsg('Some nodes chosen')


    end subroutine ClearAllNodes
    
    !----------------------------------------------------------------------
    subroutine ClearAllZones(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i
	    integer :: ncount

        if(.not. allocated(domain.Zone_Is)) then 
            allocate(domain.Zone_Is(domain.nZones),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Zone_Is array')            
            domain.Zone_Is(:)=0
        end if

        do i=1,domain.nZones
            call clear(domain.Zone_Is(i),chosen)
        end do
        
        ncount=0
        do i=1,domain.nZones
            if(bcheck(domain.Zone_Is(i),chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' Zones chosen: ',ncount
        call Msg(trim(TmpSTR))
        
	    if(ncount /= 0) call ErrMsg('Some Zones chosen')

    end subroutine ClearAllZones

    !-------------------------------------------------------------
    subroutine CLN_IaJaStructure(TMPLT_CLN)
        implicit none
        type(TecplotDomain) TMPLT_CLN
        
        integer, parameter :: MAXCONNECTIONS=20

        integer :: i, j, k, l
        integer :: iEl, jEl
        integer :: iNd, jNd
        
        real(dr) :: SeparationDistance
        
        integer :: ia_TMP(TMPLT_CLN.nElements)
        integer :: ja_TMP(MAXCONNECTIONS,TMPLT_CLN.nElements)
        !integer :: ja_TMP2(MAXCONNECTIONS,TMPLT_CLN.nElements)
        !integer :: ja_TMP2_element(MAXCONNECTIONS,TMPLT_CLN.nElements)
        !real(dr) :: ConnectionLength_TMP(MAXCONNECTIONS,TMPLT_CLN.nElements)
        !real(dr) :: PerpendicularArea_TMP(MAXCONNECTIONS,TMPLT_CLN.nElements)

        ! read and store cell connection information
        integer :: nFaceNeighborConnections
        integer :: nSizeInit
        
        !integer,allocatable :: TMPLT_CLN.Element(:)
        !integer,allocatable :: TMPLT_CLN.Face(:)
        !integer,allocatable :: TMPLT_CLN.Neighbour(:)

        integer :: iNjag
        
        call Msg(' ')
        call Msg('  Generating IA/JA and cell connection arrays for domain '//trim(TMPLT_CLN.name)//'...')
        
        ja_TMP(:,:)=0
        !ja_TMP2(:,:)=0
        !ConnectionLength_TMP(:,:)=0
        !PerpendicularArea_TMP(:,:)=0
        
        do i=1,TMPLT_CLN.nElements   ! First element connection is to itself
            ia_TMP(i)=1
            ja_TMP(ia_TMP(i),i)=-i     ! Negative entry shows start of element i list in ja 
        end do

        nSizeInit=2
        allocate(TMPLT_CLN.Element(nSizeInit),&
                TMPLT_CLN.Face(nSizeInit),&
                TMPLT_CLN.Neighbour(nSizeInit),stat=ialloc)
        call AllocChk(ialloc,'CLN element neighbour arrays')

        ! Brute force search for CLN neighbours
        nFaceNeighborConnections=0
        do iEl=1,TMPLT_CLN.nElements ! Loop over elements
            do k=1,TMPLT_CLN.nNodesPerElement ! Loop over nodes in element
                iNd=TMPLT_CLN.iNode(k,iEl)
                
                do jEl=iEl+1,TMPLT_CLN.nElements ! Loop over rest of elements
                    do l=1,TMPLT_CLN.nNodesPerElement ! Loop over nodes in next element
                        jNd=TMPLT_CLN.iNode(l,jEl)
                        SeparationDistance=sqrt((TMPLT_CLN.x(iNd) - TMPLT_CLN.x(jNd))**2 + &
                            (TMPLT_CLN.y(iNd) - TMPLT_CLN.y(jNd))**2 + &
                            (TMPLT_CLN.z(iNd) - TMPLT_CLN.z(jNd))**2 )
                        if(SeparationDistance < MinSeparationDistance) then
                            ! iEl is neighbour of jEl
                            ia_TMP(iEl)=ia_TMP(iEl)+1
                            ja_TMP(ia_TMP(iEl),iEl)=jEl
                            nFaceNeighborConnections=nFaceNeighborConnections+1
                            if(nFaceNeighborConnections > nSizeInit) then
                                call growIntegerArray(TMPLT_CLN.Element,nSizeInit,nSizeInit*2)
                                call growIntegerArray(TMPLT_CLN.Face,nSizeInit,nSizeInit*2)
                                call growIntegerArray(TMPLT_CLN.Neighbour,nSizeInit,nSizeInit*2)
                                nSizeInit=nSizeInit*2
                            end if
                            TMPLT_CLN.Element(nFaceNeighborConnections)=iEl
                            TMPLT_CLN.Face(nFaceNeighborConnections)=k
                            TMPLT_CLN.Neighbour(nFaceNeighborConnections)=jEl
                            
                            ! jEl is neighbour of iEl
                            ia_TMP(jEl)=ia_TMP(jEl)+1
                            ja_TMP(ia_TMP(jEl),jEl)=iEl
                            nFaceNeighborConnections=nFaceNeighborConnections+1
                            if(nFaceNeighborConnections > nSizeInit) then
                                call growIntegerArray(TMPLT_CLN.Element,nSizeInit,nSizeInit*2)
                                call growIntegerArray(TMPLT_CLN.Face,nSizeInit,nSizeInit*2)
                                call growIntegerArray(TMPLT_CLN.Neighbour,nSizeInit,nSizeInit*2)
                                nSizeInit=nSizeInit*2
                            end if
                            TMPLT_CLN.Element(nFaceNeighborConnections)=jEl
                            TMPLT_CLN.Face(nFaceNeighborConnections)=l
                            TMPLT_CLN.Neighbour(nFaceNeighborConnections)=iEl
                        endif
                    end do
                end do
            end do
        end do

        ! Determine size of ja (njag) and copy ja_TMP to ja 
        TMPLT_CLN.njag=0
        do i=1,TMPLT_CLN.nElements
            TMPLT_CLN.njag=TMPLT_CLN.njag+ia_TMP(i)
        end do
        allocate(TMPLT_CLN.ia(TMPLT_CLN.nElements),TMPLT_CLN.ja(TMPLT_CLN.njag),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_CLN.name)//'CLN ia, ja array')
        !allocate(TMPLT_CLN.ConnectionLength(TMPLT_CLN.njag),TMPLT_CLN.PerpendicularArea(TMPLT_CLN.njag),stat=ialloc)
        !call AllocChk(ialloc,'SWF Cell connection length, perpendicular area array')
        !TMPLT_CLN.ConnectionLength(:)=0.0d0
        !TMPLT_CLN.PerpendicularArea(:)=0.0d0
        TMPLT_CLN.ia=ia_TMP
        
        
        iNJag=0
        do i=1,TMPLT_CLN.nElements
            do j=1,TMPLT_CLN.ia(i)
                iNjag=iNjag+1
                TMPLT_CLN.ja(iNJag)=ja_TMP(j,i)
                !TMPLT_CLN.jaElement(iNJag)=ja_TMP2_Element(j,i)
                !TMPLT_CLN.ConnectionLength(iNjag)=ConnectionLength_TMP(j,i)
                !TMPLT_CLN.PerpendicularArea(iNjag)=PerpendicularArea_TMP(j,i)
            end do
        end do
           
        return
    end subroutine CLN_IaJaStructure
    
    !-------------------------------------------------------------
    subroutine CLN_Read(Modflow)
        implicit none

        type (ModflowProject) Modflow
        character(400) :: line
        
        integer :: ICLNNDS
        CHARACTER*24 ANAME(3)
        DATA ANAME(1) /'   NODES PER CLN SEGMENT'/
        DATA ANAME(2) /'                      IA'/
        DATA ANAME(3) /'                      JA'/

        integer :: i1
        integer :: IJA
        integer :: II
        real :: FLENG
        integer :: IFTYP
        integer :: ICCWADI
        real :: FELEV
        integer :: IFDIR
        integer :: IFNO
        real :: FANGLE
        integer :: IFLIN
        integer :: LLOC
        integer :: ISTART
        integer :: ISTOP
        real :: R
        real :: FSKIN        
        real :: FANISO        
        integer :: IFCON
        integer :: IFNOD
        integer :: ICGWADI
        integer :: IFROW
        integer :: IFLAY
        integer :: IFCOL, i, j, k
     
        
        
        IOUT=FNumEco
        INCLN=Modflow.iCLN
        
        WRITE(IOUT,1)
1       FORMAT(1X,/1X,'CLN -- CONNECTED LINE NETWORK DISCRETIZATION PROCESS, VERSION 1, 3/3/2012 ')
        
        do 
            do 
                read(Modflow.iCLN,'(a)') line
                if(line(1:1).eq.'#') then
                    write(*,'(a)') line
                    cycle
                end if
                backspace(Modflow.iCLN)
                exit
            end do
            
            read(Modflow.iCLN,'(a)',iostat=status) line
            call LwrCse(line)
            if(status /= 0) then
                call ErrMsg('While reading CLN')
            endif
            
            IF(index(line,'options') .ne. 0) THEN
                IF(index(line,'transient') .ne. 0) THEN
                    ICLNTIB=1
                    WRITE(IOUT,71)
71                  FORMAT(1X,'TRANSIENT IBOUND OPTION: READ TRANSIENT IBOUND RECORDS FOR EACH STRESS PERIOD.')
                    
                end if    
            
                IF(index(line,'printiaja') .ne. 0) THEN
                    IPRCONN=1
                    WRITE(IOUT,72)
72                  FORMAT(1X,'PRINT CLN IA AND JA OPTION: THE CLN IA AND JA ARRAYS WILL BE PRINTED TO LIST FILE.')
                    
                end if
            
                IF(index(line,'processccf') .ne. 0) THEN
                    read(Modflow.iCLN,*) ICLNGWCB

                    ICLNPCB=1                                                     !aq CLN CCF
                    WRITE(IOUT,73)                                                !aq CLN CCF
73                  FORMAT(1X,'PROCESS CELL-TO-CELL FLOW BUDGET OPTION: FLOW BUDGET WILL USE A SEPARATE FILE FOR CLN-GWF FLOW.')     !aq CLN CCF
                    
                    IF(ICLNGWCB.LT.0) WRITE(IOUT,18)                              !aq CLN CCF
18                  FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE PRINTED WHEN ICBCFL IS NOT 0 (FLAG ICLNGWCB IS LESS THAN ZERO)')                  !aq CLN CCF
                    
                    IF(ICLNGWCB.GT.0) WRITE(IOUT,19)                      !aq CLN CCF
19                  FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE SAVED(FLAG ICLNGWCB IS GREATER THAN ZERO)')                         !aq CLN CCF
                    
                end if
                
            else
                if(line(1:1)=='#')then 
                    read(Modflow.iCLN,'(a)',iostat=status) line
                endif
                read(line,*) NCLN,ICLNNDS,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNGWC,NCONDUITYP
                WRITE(IOUT,3) NCLN,ICLNNDS,NCLNGWC                                             
3               FORMAT(1X,'FLAG (0) OR MAXIMUM NUMBER OF LINEAR NODES (NCLN) =',I7&     
                    /1X,'FLAG (-VE) OR NUMBER OF LINEAR NODES (+VE)',&     
                    1X,'(ICLNNDS) =',I7&     
                    /1X,'NUMBER OF LINEAR NODE TO MATRIX GRID CONNECTIONS',&     
                    ' (NCLNGWC) =',I7/) 
                
                IF(ICLNCB.LT.0) WRITE(IOUT,7)
7               FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',       &
                    ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')

                IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
8               FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,         &
                    '(FLAG ICLNCB IS GREATER THAN ZERO)')

                IF(ICLNCB.EQ.0) WRITE(IOUT,6)
6               FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',       &
                    1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')

                IF(ICLNHD.LT.0) WRITE(IOUT,9)
9               FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,      &
                    'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX',   &!kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNHD IS LESS THAN ZERO)')

                IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
10              FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,             &
                    '(FLAG ICLNHD IS GREATER THAN ZERO)')

                IF(ICLNHD.EQ.0) WRITE(IOUT,31)
31              FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',           &
                    1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')

                IF(ICLNDD.LT.0) WRITE(IOUT,12)
12              FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,      &
                    'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNDD IS LESS THAN ZERO)')

                IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
                13  FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,              &
                    '(FLAG ICLNDD IS GREATER THAN ZERO)')

                IF(ICLNDD.EQ.0) WRITE(IOUT,14)
14              FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',            &
                    1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')

                IF(ICLNIB.LT.0) WRITE(IOUT,32)
32              FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,    &
                    'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
                

                IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
33              FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,           &
                    '(FLAG ICLNIB IS GREATER THAN ZERO)')
                

                IF(ICLNIB.EQ.0) WRITE(IOUT,17)
17              FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',         &
                    1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
                
                !C--------------------------------------------------------------------------------
                !C3B----READ GRAVITY AND KINEMATIC VISCOSITY IN CASE IT IS REQUIRED FOR TURBULENT FLOW
                !ALLOCATE(GRAV,VISK)
                !ALLOCATE(IBHETYP)
                GRAV = 0.0
                VISK = 0.0
                IBHETYP = 0
                
                IF(index(line,'gravity') .ne. 0) THEN
                    i1=index(line,'gravity')+7
                    line=line(i1:)
                    read(line,*) GRAV
                    WRITE(IOUT,34) GRAV
34                  FORMAT(1X,'GRAVITATIONAL ACCELERATION [L/T^2] = ', G15.6)
                end if 
                
                IF(index(line,'viscosity') .ne. 0) THEN
                    i1=index(line,'viscosity')+9
                    line=line(i1:)
                    read(line,*) VISK
                    WRITE(IOUT,35) VISK
35                  FORMAT(1X,'KINEMATIC VISCOSITY [L^2/T] = ', G15.6)
                END IF

                !C3B----READ OPTION FOR NON-CIRCULAR CROSS-SECTIONS
                IF(index(line,'rectangular') .ne. 0) THEN
                    i1=index(line,'rectangular')+11
                    line=line(i1:)
                    read(line,*) NRECTYP
                    WRITE(IOUT,36) NRECTYP
36                  FORMAT(1X,'NUMBER OF RECTANGULAR SECTION GEOMETRIES = ', I10)
                END IF

                !C3C----READ OPTION FOR BHE DETAILS
                IF(index(line,'bhedetail') .ne. 0) THEN
                    IBHETYP = 1
                    IF(ITRNSP.EQ.0) IBHETYP = 0 ! NO BHE (OR INPUT) IF TRANSPORT IS NOT RUN
                    WRITE(IOUT,37)
37                  FORMAT(1X,'BHE DETAILS WILL BE INPUT FOR EACH CLN TYPE')
                end if

                !C3D----READ OPTION FOR SAVING CLN OUTPUT AND UNIT NUMBER
                IF(index(line,'saveclncon') .ne. 0) THEN
                    i1=index(line,'saveclncon')+10
                    line=line(i1:)
                    read(line,*) ICLNCN
                    
                    !IF(INBCT.EQ.0) ICLNCN = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
                    ICLNCN = 0 ! SHUT OFF for now rgm
                    
                    IF(ICLNCN.LT.0) WRITE(IOUT,42)
42                  FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,    &
                    'NUMBER (ISPCUN) AS USED FOR CONC OUTPUT FOR POROUS MATRIX',     & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNCN IS LESS THAN ZERO)')
                    
                    IF(ICLNCN.GT.0) WRITE(IOUT,43) ICLNCN
43                  FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED ON UNIT ',I4,      &
                   '(FLAG ICLNCN IS GREATER THAN ZERO)')
                    
                    IF(ICLNCN.EQ.0) WRITE(IOUT,44)
44                  FORMAT(1X,'CLN CONC OUTPUT WILL NOT BE SAVED OR PRINTED',       &
                     1X,'(FLAG ICLNCN IS EQUAL TO ZERO)')
                    
                end if
!
                IF(index(line,'saveclnmas') .ne. 0) THEN
                    i1=index(line,'saveclnmas')+10
                    line=line(i1:)
                    read(line,*) ICLNMB
                    !IF(INBCT.EQ.0) ICLNMB = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
                    ICLNMB = 0 ! SHUT OFF for now rgm
                end if

                IF(ICLNMB.LT.0) WRITE(IOUT,45)
45              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED TO THE SAME',1X, &
                'UNIT NUMBER (IBCTCB) AS USED FOR CONC OUTPUT FOR POROUS',   & !kkz - added trailing comma per JCH
                1X,'MATRIX (FLAG ICLNMB IS LESS THAN ZERO)')
                
                IF(ICLNMB.GT.0) WRITE(IOUT,46) ICLNMB
46              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED ON UNIT ',I4,         &
                '(FLAG ICLNMB IS GREATER THAN ZERO)')
        
                IF(ICLNMB.EQ.0) WRITE(IOUT,47)
47              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL NOT BE SAVED OR PRINTED',          &
                1X,'(FLAG ICLNMB IS EQUAL TO ZERO)')
            END IF

            !C--------------------------------------------------------------------------------
            !C4------FOR INPUT OF MULTI-NODE WELLS OR CLN SEGMENTS
            !C4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
            IF(NCLN.GT.0)THEN
        !        ALLOCATE(NNDCLN(0:NCLN))
        !        K = 0
        !        CALL U1DINT(NNDCLN(1),ANAME(1),NCLN,K,IOUT,IOUT)
        !        NNDCLN(0) = 0
        !C
        !C5--------MAKE NNDCLN ARRAY CUMULATIVE
        !        DO I = 1,NCLN
        !          NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
        !        end do
        !        NCLNCONS = NNDCLN(NCLN)
        !C------------------------------------------------------------------------------
        !C6--------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
        !        IF(ICLNNDS.LT.0)THEN
        !C6A---------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
        !          NCLNNDS = NNDCLN(NCLN)
        !          ALLOCATE(CLNCON(NCLNNDS))
        !          DO I=1,NCLNNDS
        !            CLNCON(I) = I ! +NODES  ! (KEEP LOCAL NODE NUMBER)
        !          end do
        !        ELSE
        !C6B-------SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
        !          NCLNNDS = ICLNNDS
        !          ALLOCATE(CLNCON(NCLNCONS))
        !          DO I=1,NCLN
        !            IF(IFREFM.EQ.0) THEN
        !              read(Modflow.iCLN,'(200I10)')
        !     1        (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
        !            ELSE
        !              read(Modflow.iCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
        !            end if
        !          end do
        !cspC6C---------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
        !csp          DO I=1,NCLNCONS
        !csp            CLNCON(I) = NODES + CLNCON(I)
        !csp          end do
        !        end if
        !C6D--------CONVERT TO IA_CLN AND JA_CLN
        !        ALLOCATE(IA_CLN(NCLNNDS+1))
        !        CALL FILLIAJA_CLN
        !C6E---------DEALLOCATE UNWANTED ARRAYS
        !        DEALLOCATE (NNDCLN) ! NNDCLN NEEDED FOR WRITING BUDGET TO ASCII FILE?
        !        DEALLOCATE (CLNCON)
            ELSE
                !C----------------------------------------------------------------------
                !C7------FOR INPUT OF IA AND JAC OF CLN DOMAIN (NCLN = 0), READ DIRECTLY
                NCLNNDS = ICLNNDS
                ALLOCATE(IA_CLN(NCLNNDS+1))
                !C7A-------READ NJA_CLN
                IF(IFREFM.EQ.0) THEN
                  read(Modflow.iCLN,'(I10)') NJA_CLN
                ELSE
                  read(Modflow.iCLN,*) NJA_CLN
                end if
                !C7B-------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA_CLN AND JA_CLN ARRAYS
                K = 0
                CALL U1DINT(IA_CLN,ANAME(2),NCLNNDS,K,INCLN,IOUT)
                ALLOCATE(JA_CLN(NJA_CLN))
                CALL U1DINT(JA_CLN,ANAME(3),NJA_CLN,K,INCLN,IOUT)
                !C7C--------ENSURE POSITIVE TERM FOR DIAGONAL OF JA_CLN
                DO IJA = 1,NJA_CLN
                  IF(JA_CLN(IJA).LT.0) JA_CLN(IJA) = -JA_CLN(IJA)
                end do
                !C7D--------MAKE IA_CLN CUMULATIVE FROM CONNECTION-PER-NODE
                DO II=2,NCLNNDS+1
                  IA_CLN(II) = IA_CLN(II) + IA_CLN(II-1)
                end do
                !C---------IA_CLN(N+1) IS CUMULATIVE_IA_CLN(N) + 1
                DO II=NCLNNDS+1,2,-1
                  IA_CLN(II) = IA_CLN(II-1) + 1
                end do
                IA_CLN(1) = 1
            end if
            !C----------------------------------------------------------------------
            !C8------ALLOCATE SPACE FOR CLN PROPERTY ARRAYS
            ALLOCATE(ACLNNDS(NCLNNDS,6))
            ALLOCATE(IFLINCLN(NCLNNDS))
            ALLOCATE(ICCWADICLN(NCLNNDS))
            ALLOCATE(ICGWADICLN(NCLNGWC))
            !C
            !C9------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
             WRITE(IOUT,21)
21           FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/&
                20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,& 
               'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,&
               'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),&
                4X,11('-'),8X,6('-'),4X,7('-'))
            !C
            !C10-------READ BASIC PROPERTIES FOR ALL CLN NODES AND FILL ARRAYS
            DO I = 1,NCLNNDS
                CALL URDCOM(INCLN,IOUT,LINE)
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
                    !READ(LINE,*) IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
                    LLOC=71
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFDIR,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANGLE,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLIN,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCWADI,R,IOUT,IOUT)
                END IF
                IF(IFLIN.EQ.0) IFLIN = -1
                !C11A-------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
                !c        IF(IFDIR.EQ.2)THEN
                !c          FDPTH = FLENG * SIN(FANGLE)
                !c          IC=IFTYP
                !c          CALL CLNR(IC,FRAD)
                !c          IF(FDPTH.LT.2.0*FRAD) IFDIR = 1
                !c        end if
                WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
22              FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)

                !C11B------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
                ACLNNDS(I,1) = IFNO + NODES ! GLOBAL NODE NUMBER FOR CLN-CELL
                ACLNNDS(I,2) = IFTYP
                ACLNNDS(I,3) = IFDIR
                ACLNNDS(I,4) = FLENG
                ACLNNDS(I,5) = FELEV
                ACLNNDS(I,6) = FANGLE
                IFLINCLN(I) = IFLIN
                ICCWADICLN(I) = ICCWADI
            END DO
            !----------------------------------------------------------------------------------------
            !12------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
            ALLOCATE(ACLNGWC(NCLNGWC,6))
            !----------------------------------------------------------------------------------------
            !13------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
            IF(IUNSTR.EQ.0)THEN
                !
                !14A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
                !14A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION

                !1------PREPARE TO REFLECT INPUT INTO LISTING FILE
               WRITE(IOUT,41)
41             FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/&
                    20X,40('-')/5X,'F-NODE NO.',6X,'LAYER',8X,'ROW',5X,'COLUMN',&
                    2X,'EQTN. TYPE',5X,'      FSKIN',11X,'FLENG',10X,&
                    'FANISO',3X,'ICGWADI'/5X,10('-'),6X,5('-'),8X,3('-'),5X,&
                    6('-'),2X,11('-'),3X,12('-'),2X,14('-'),4X,12('-'),3X,7('-'))
                
                !2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
                DO I = 1,NCLNGWC
                    CALL URDCOM(INCLN,IOUT,LINE)
                    IF(IFREFM.EQ.0) THEN
                        READ(LINE,*) IFNO,IFLAY,IFROW,IFCOL,IFCON,&
                        FSKIN,FLENG,FANISO,ICGWADI
                        LLOC=91
                    ELSE
                        LLOC=1
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLAY,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFROW,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCOL,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
                    END IF
                    !3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
                    IF(IFCON.EQ.0)FSKIN = 0.0
                    WRITE(IOUT,52)IFNO,IFLAY,IFROW,IFCOL,IFCON,FSKIN,FLENG,FANISO,&
                            ICGWADI
52                          FORMAT(5X,I10,3(1X,I10),2X,I10,3(1X,E15.6),1X,I9)
                            
                        
                    !4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
                    ACLNGWC(I,1) = IFNO
                    IFNOD = (IFLAY-1)*NROW*NCOL + (IFROW-1)*NCOL + IFCOL
                    ACLNGWC(I,2) = IFNOD
                    ACLNGWC(I,3) = IFCON
                    ACLNGWC(I,4) = FSKIN
                    ACLNGWC(I,5) = FANISO
                    ACLNGWC(I,6) = FLENG
                    ICGWADICLN(I) = ICGWADI
                end do
            ELSE
                !
                !14B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
                !14B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION

                !1------PREPARE TO REFLECT INPUT INTO LISTING FILE
                WRITE(IOUT,23)
23              FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/&
                    20X,40('-')/5X,'F-NODE NO.',1X,'GW-NODE NO',2X,&
                    'EQTN. TYPE',2X,'      FSKIN',11X,&
                    'FLENG',9X,'FANISO'3X,'ICGWADI'/5X,10('-'),1X,10('-'),&
                    1X,11('-'),5X,11('-'),1X,17('-'),1X,15('-'),3X,10('-'))
                
                !2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
                DO I = 1,NCLNGWC
                    CALL URDCOM(INCLN,IOUT,LINE)
                    IF(IFREFM.EQ.0) THEN
                        READ(LINE,'(3I10,3F10.3,I10)') IFNO,IFNOD,IFCON,FSKIN,FLENG,&
                            FANISO,ICGWADI
                        LLOC=71
                    ELSE
                        LLOC=1
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNOD,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
                    END IF
                    !3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
                    IF(IFCON.EQ.0)FSKIN = 0.0
                    WRITE(IOUT,24)IFNO,IFNOD,IFCON,FSKIN,FLENG,FANISO,ICGWADI
24                  FORMAT(5X,I10,1X,I10,2X,I10,3(1X,E15.6),1X,I9)
                    
                    !4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
                    ACLNGWC(I,1) = IFNO
                    ACLNGWC(I,2) = IFNOD
                    ACLNGWC(I,3) = IFCON
                    ACLNGWC(I,4) = FSKIN
                    ACLNGWC(I,5) = FANISO
                    ACLNGWC(I,6) = FLENG
                    ICGWADICLN(I) = ICGWADI
                end do
                
            end if
            !!----------------------------------------------------------------------------------------
            !!15B------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
            !      IF(NRECTYP.GT.0)THEN
            !        CALL SCLN2REC1RP
            !      end if
            !!----------------------------------------------------------------------------------------
            !!16------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
            !!ADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
            !!----------------------------------------------------------------------------------------
               
            WRITE(IOUT,'(/,A)')' IA_CLN IS BELOW, 40I10'
            WRITE(IOUT,55)(IA_CLN(I),I=1,NCLNNDS+1)
            WRITE(IOUT,*)'NJA_CLN = ',NJA_CLN
            WRITE(IOUT,*)'JA_CLN IS BELOW, 40I10'
            WRITE(IOUT,55)(JA_CLN(J),J=1,NJA_CLN)
55          FORMAT(40I10)
            
            exit
            
        end do
        RETURN
    end subroutine CLN_Read
!----------------------------------------------------------------------------------------
    SUBROUTINE CLN_ReadCircularProperties(Modflow)
!     ******************************************************************
!      ALLOCATE SPACE AND READ PROPERTIES FOR CONDUIT TYPE CLNs
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IOUT,IFREFM,&
                       INCLN
        implicit none
      DOUBLE PRECISION PERIF,AREAF
      CHARACTER*400 LINE

        type (ModflowProject) Modflow
        
        integer :: i, ifno, lloc
        real :: conduitk, frad, tcond, tcoef, R
        integer :: istop, istart
        real :: tthk, tcfluid, fsrad
        
      
!----------------------------------------------------------------------------------------
!12------ALLOCATE SPACE FOR CONDUIT TYPE CLNs AND PREPARE TO REFLECT INPUT TO LISTING FILE
      ALLOCATE (ACLNCOND(NCONDUITYP,5))
      ALLOCATE (BHEPROP(NCONDUITYP,4))
      BHEPROP = 0.0
      IF(IBHETYP.EQ.0) THEN
        WRITE(IOUT,23)
23      FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
       20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
       /5X,12('-'),8X,6('-'),3X,13('-'))
      ELSE
        WRITE(IOUT,24)
24      FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
       20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
       5X,'COND. PIPE', 6X,'THICK PIPE',5X,'COND. FLUID',&
       5X,'CONV. COEFF',&
       /5X,12('-'),4X,10('-'),3X,13('-'))
      ENDIF
!13------READ CONDUIT PROPERTIES FOR EACH CONDUIT TYPE
      DO I=1,NCONDUITYP
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          IF(IBHETYP.EQ.0)THEN
            READ(LINE,'(I10,2F10.3)') IFNO,FRAD,CONDUITK
            LLOC=71
          ELSE
            READ(LINE,'(I10,6F10.3)')IFNO,FRAD,CONDUITK,TCOND,TTHK, &
              TCFLUID,TCOEF
            LLOC=111
          ENDIF
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSRAD,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONDUITK,IOUT,INCLN)
          FRAD = FSRAD
          IF(IBHETYP.EQ.1)THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOND,IOUT,INCLN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTHK,IOUT,INCLN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCFLUID,IOUT,INCLN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOEF,IOUT,INCLN)
          ENDIF
        END IF
!
!14--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ACLNCOND(I,1) = IFNO
        ACLNCOND(I,2) = FRAD
        ACLNCOND(I,3) = CONDUITK
        CALL CLNA(IFNO,AREAF)
        ACLNCOND(I,4) = AREAF
        CALL CLNP(I,PERIF)
        ACLNCOND(I,5) = PERIF
        IF(IBHETYP.EQ.1)THEN
          BHEPROP(I,1) = TCOND
          BHEPROP(I,2) = TTHK
          BHEPROP(I,3) = TCFLUID
          BHEPROP(I,4) = TCOEF
          WRITE(IOUT,26) IFNO,FRAD,CONDUITK,TCOND,TTHK,TCFLUID,TCOEF
26        FORMAT(5X,I10,6(1X,E15.6))
        ELSE
        WRITE(IOUT,25)IFNO,FRAD,CONDUITK
25      FORMAT(5X,I10,2(1X,E15.6))
        ENDIF
      ENDDO
!--------RETURN
      RETURN
      END subroutine CLN_ReadCircularProperties

!----------------------------------------------------------------------------
      SUBROUTINE CLN_ReadRectangularProperties(Modflow)
!     ******************************************************************
!      ALLOCATE SPACE AND READ PROPERTIES FOR RECTAQNGULAR TYPE CLNs
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IOUT,IFREFM,&
                       INCLN
        implicit none
      DOUBLE PRECISION PERIF,AREAF
      CHARACTER*400 LINE

        type (ModflowProject) Modflow
        
        integer :: i, ifno, lloc, istart, istop, iftotno
        real :: fheight, flength, conduitk, r, fsw, fsh, fwidth
        
        
!----------------------------------------------------------------------------------------
!12------ALLOCATE SPACE FOR CONDUIT TYPE CLNs AND PREPARE TO REFLECT INPUT TO LISTING FILE
      ALLOCATE (ACLNREC(NRECTYP,6))
      WRITE(IOUT,23)
23    FORMAT(/20X,' RECTANGULAR CLN SECTION INFORMATION'/&
       20X,40('-')/5X,'RECTANGULAR NODE',8X,'LENGTH',8X,'HEIGHT',3X,&
      'CONDUIT SAT K' /5X,12('-'),8X,6('-'),8X,6('-'),3X,13('-'))
!13------READ RECTANGULAR GEOMETRY PROPERTIES FOR EACH RECTANGULAR TYPE
      DO I=1,NRECTYP
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(I10,3F10.3)') IFNO,FLENGTH,FHEIGHT,CONDUITK
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSW,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSH,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONDUITK,IOUT,INCLN)
          FWIDTH = FSW
          FHEIGHT = FSH
        END IF
!
!14--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        IFTOTNO = IFNO + NCONDUITYP
        ACLNREC(I,1) = IFTOTNO
        ACLNREC(I,2) = FWIDTH
        ACLNREC(I,3) = FHEIGHT
        ACLNREC(I,4) = CONDUITK
        CALL CLNA(IFTOTNO,AREAF)
        ACLNREC(I,5) = AREAF
        CALL CLNP(IFTOTNO,PERIF)
        ACLNREC(I,6) = PERIF
        WRITE(IOUT,24)IFNO,FWIDTH,FHEIGHT,CONDUITK
24      FORMAT(5X,I10,3(1X,E15.6))
      ENDDO
!--------RETURN
      RETURN
      END subroutine CLN_ReadRectangularProperties
      
    !-------------------------------------------------------------
    subroutine CLNToTecplot(Modflow,TMPLT_CLN)
        implicit none
        type(ModflowProject) Modflow
        type(TecplotDomain) TMPLT_CLN

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j

        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(modflow.CLN.name)//'.tecplot.dat'
        
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(TMPLT_CLN.name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","'//trim(modflow.CLN.name)//' Zone","'//trim(modflow.CLN.name)//' zCell",'
        nVar=5
            
        if(allocated(Modflow.CLN.Sgcl)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.CLN.name)//' CLN-GWF connection length",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.CLN.StartingHeads)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.CLN.name)//' Initial Depth",'
            nVar=nVar+1
        end if
        
        if(allocated(Modflow.CLN.CellArea)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.CLN.name)//' Cell area",'
            nVar=nVar+1
        end if
                
        write(FNum,'(a)') trim(VarSTR)
          
        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(modflow.CLN.name)//'"  ,N=',TMPLT_CLN.nNodes,', E=',TMPLT_CLN.nElements,&
        ', datapacking=block, zonetype='//trim(TMPLT_CLN.elementtype)
            
        if(modflow.NodalControlVolume) then
            write(FNum,'(a)') trim(ZoneSTR) 
        else
            CellCenteredSTR=', VARLOCATION=([4'
            if(nVar.ge.5) then
                do j=5,nVar
                    if(.not. Modflow.NodalControlVolume) then  ! z Cell is not CELLCENTERED
                        write(str2,'(i2)') j
                        CellCenteredSTR=trim(CellCenteredSTR)//','//str2
                    end if
                end do
            end if
            CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'

            write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR) 
        end if
        
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (TMPLT_CLN.x(i),i=1,TMPLT_CLN.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (TMPLT_CLN.y(i),i=1,TMPLT_CLN.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (TMPLT_CLN.z(i),i=1,TMPLT_CLN.nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (Modflow.CLN.iZone(i),i=1,Modflow.CLN.nCells)
            
        write(FNum,'(a)') '# zCell i.e. cell bottom'
        write(FNum,'(5e20.12)') (Modflow.CLN.zCell(i),i=1,Modflow.CLN.nCells)
            
        if(allocated(Modflow.CLN.Sgcl)) then
            write(FNum,'(a)') '# SW-GW connection length'
            write(FNum,'(5e20.12)') (Modflow.CLN.Sgcl(i),i=1,Modflow.CLN.nCells)
        end if

        if(allocated(Modflow.CLN.StartingHeads)) then
            write(FNum,'(a)') '# Starting depth'
            write(FNum,'(5e20.12)') (Modflow.CLN.StartingHeads(i)-Modflow.CLN.ZCell(i),i=1,Modflow.CLN.nCells)
        end if

        if(allocated(Modflow.CLN.CellArea)) then
            write(FNum,'(a)') '# Cell Area'
            write(FNum,'(5e20.12)') (Modflow.CLN.CellArea(i),i=1,Modflow.CLN.nCells)
        end if
        
        do i=1,TMPLT_CLN.nElements
            if(TMPLT_CLN.nNodesPerElement==2) then ! 2-node line, repeat node 3 for 4-node tecplot type fequadrilateral
                write(FNum,'(8i8)') (TMPLT_CLN.iNode(j,i),j=1,2) !, TMPLT_CLN.iNode(3,i) 
            else
                write(TmpSTR,'(i2)') TMPLT_CLN.nNodesPerElement
                call ErrMsg(trim(TMPLT_CLN.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if
        end do
       
        call FreeUnit(FNum)
        
    end subroutine CLNToTecplot

    !-------------------------------------------------------------
    subroutine CreateStepPeriodTimeFile(Modflow)
        implicit none

        type (ModflowProject) Modflow
        
        integer :: Fnum
        integer :: FNumStepPeriodTime
        character(MAX_STR) :: FNameStepPeriodTime
        
        
        integer :: iTStep
        integer :: iPeriod
        real(dr) :: TotalTime
        real(dr) :: dum1, dum2, dum3, dum4
        
        character(4000) :: line

        FNum=Modflow.iLIST
        rewind(FNum)

        FNameStepPeriodTime=trim(Modflow.MUTPrefix)//'o.StepPeriodTime'
        call OpenAscii(FNumStepPeriodTime,FNameStepPeriodTime)
        call Msg( 'Time step, stress period, time to file: '//trim(FNameStepPeriodTime))

        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) return
            
            if(index(line,'TIME SUMMARY AT END OF TIME STEP').gt.0) then
                line=line(index(line,'STEP')+5:)
                read(line,*) iTStep
                line=line(index(line,'PERIOD')+7:)
                read(line,*) iPeriod
                
                loop: do
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    if(index(line,'TOTAL TIME').gt.0) then
                        l1=index(line,'TOTAL TIME')
                        if(index(Modflow.STR_TimeUnit,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'YEARS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, dum4, TotalTime
                        end if
                        exit loop
                    end if
                end do loop
                
                write(FNumStepPeriodTime,*) iTStep, iPeriod, TotalTime
            end if
        end do
                
    end subroutine CreateStepPeriodTimeFile

    !-------------------------------------------------------------
    subroutine DisplayDomainAttributes(domain)
        implicit none
        type (TecplotDomain) Domain
        
        
        call Msg('*** Derived '//trim(domain.name)//' mesh attributes:')
        call Msg('    Tecplot Element Type: '//trim(domain.ElementType))
        write(TMPStr,'(i10)') domain.nElements
        call Msg(trim(TmpSTR)//' Elements')
        write(TMPStr,'(i10)') domain.nLayers
        call Msg(trim(TmpSTR)//' Layers')
        write(TMPStr,'(i10)') domain.nNodes
        call Msg(trim(TmpSTR)//' Nodes')
        write(TMPStr,'(i10)') domain.nNodesPerElement
        call Msg(trim(TmpSTR)//' Nodes Per Element')
        
        return
    end  subroutine DisplayDomainAttributes

    !!-------------------------------------------------------------
    !subroutine FindNeighbours(Modflow,domain)
    !    implicit none
    !    type (ModflowProject) Modflow
    !    type(ModflowDomain) domain
    !    
    !    integer :: i
    !    
    !    if(domain.name == 'GWF') then
    !        if(domain.nNodesPerElement==6) then 
    !            domain.nEdgesPerCell=9
    !        else if(domain.nNodesPerElement==8) then
    !            domain.nEdgesPerCell=12
    !        else
    !            write(TmpSTR,'(i2)') Modflow.GWF.nNodesPerElement
    !            call ErrMsg('Modflow.GWF.nNodesPerElement not supported yet: '//trim(TmpSTR))
    !        end if
    !        allocate(Modflow.GWF.xEdge(Modflow.GWF.nCells,Modflow.GWF.nNodesPerElement),&
    !                Modflow.GWF.yEdge(Modflow.GWF.nCells,Modflow.GWF.nNodesPerElement),&
    !                Modflow.GWF.zEdge(Modflow.GWF.nCells,Modflow.GWF.nNodesPerElement),stat=ialloc)
    !        call AllocChk(ialloc,'GWF edge centroid arrays')
    !    else if(domain.name == 'SWF') then
    !        if(Modflow.SWF.nNodesPerElement==3) then ! Assume 4-node rectangular element output for Tecplot repeat node 3
    !            Modflow.SWF.nEdgesPerCell=3
    !        else if(Modflow.SWF.nNodesPerElement==4) then
    !            Modflow.SWF.nEdgesPerCell=4
    !        else
    !            write(TmpSTR,'(i2)') Modflow.SWF.nNodesPerElement
    !            call ErrMsg('Modflow.SWF.nNodesPerElement not supported yet: '//trim(TmpSTR))
    !        end if
    !        allocate(Modflow.SWF.xEdge(Modflow.SWF.nCells,Modflow.SWF.nEdgesPerCell),&
    !                Modflow.SWF.yEdge(Modflow.SWF.nCells,Modflow.SWF.nEdgesPerCell),&
    !                Modflow.SWF.zEdge(Modflow.SWF.nCells,Modflow.SWF.nEdgesPerCell),stat=ialloc)
    !        call AllocChk(ialloc,'SWF edge centroid arrays')
    !    end if
    !    
    !    
    !    do i=1,domain.nCells
    !    if(domain.name == 'GWF') then
    !    else if(domain.name == 'SWF') then
    !        domain.xEdge(i,1)=(domain.x(i,1)+domain.x(i,2))/2.0d0
    !        domain.yEdge(i,1)=(domain.y(i,1)+domain.y(i,2))/2.0d0
    !        domain.zEdge(i,1)=(domain.z(i,1)+domain.z(i,2))/2.0d0
    !            
    !        domain.xEdge(i,2)=(domain.x(i,2)+domain.x(i,3))/2.0d0
    !        domain.yEdge(i,2)=(domain.y(i,2)+domain.y(i,3))/2.0d0
    !        domain.zEdge(i,2)=(domain.z(i,2)+domain.z(i,3))/2.0d0
    !            
    !        domain.xEdge(i,3)=(domain.x(i,3)+domain.x(i,1))/2.0d0
    !        domain.yEdge(i,3)=(domain.y(i,3)+domain.y(i,1))/2.0d0
    !        domain.zEdge(i,3)=(domain.z(i,3)+domain.z(i,1))/2.0d0
    !    end if    
    !    end do
    !    
    !    do i=1,nCells
    !        do j=i,nCells
    !            
    !    
    !    call FreeUnit(FNum)
    !
    !end subroutine FindNeighbours

    !----------------------------------------------------------------------
    subroutine FlagChosenCellsInactive(domain) 
        implicit none

        type (ModflowDomain) Domain
        
        integer :: i

        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                call set(domain.Cell_Is(i),Inactive)
            end if
        end do
    
    end subroutine FlagChosenCellsInactive

     !----------------------------------------------------------------------
    subroutine FlagChosenCellsInactiveTMPLT(TMPLT) 
        implicit none

        type (TecplotDomain) TMPLT
        
        integer :: i

        do i=1,TMPLT.nElements
            if(bcheck(TMPLT.Element_is(i),chosen)) then
                call set(TMPLT.Element_Is(i),Inactive)
            end if
        end do
    
    end subroutine FlagChosenCellsInactiveTMPLT
    
    !----------------------------------------------------------------------
    subroutine FlagChosenNodesAsOuterBoundary(domain) 
        implicit none

        type (ModflowDomain) Domain
        
        integer :: i

        do i=1,domain.nNodes
            if(bcheck(domain.Node_is(i),chosen)) then
                call set(domain.Node_Is(i),BoundaryNode)
            end if
        end do
    
    end subroutine FlagChosenNodesAsOuterBoundary

    !----------------------------------------------------------------------
    subroutine FlagChosenNodesAsOuterBoundaryTMPLT(TMPLT) 
        implicit none

        type (TecplotDomain) TMPLT
        
        integer :: i

        do i=1,TMPLT.nNodes
            if(bcheck(TMPLT.Node_is(i),chosen)) then
                call set(TMPLT.Node_Is(i),BoundaryNode)
            end if
        end do
    
    end subroutine FlagChosenNodesAsOuterBoundaryTMPLT

    !-------------------------------------------------------------
    subroutine FlipHGSNumsBottomToTop(Hgs)
        implicit none
        type (HgsProject) Hgs
        
        integer :: i, nsheet
        
        ! Node number 
        do i=1,1375
            nSheet=int(i/hgs.nn2d)
            write(*,*) i,nsheet
        end do
    end subroutine FlipHGSNumsBottomToTop

    !-------------------------------------------------------------
    subroutine GenOCFile(FNumMUT,Modflow)
        implicit none
        
        character(MAX_INST) :: instruction

        integer :: FNumMUT
        
        integer :: i
        
        real :: OutputTimes(1000)
        
        type (ModflowProject) Modflow
        
        modflow.nOutputTimes=0
        
	    ! Change default behaviours and top elevation
        read_oc: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            if(index(instruction, 'end') /=0) then
                call Msg('end generate output control file')
                exit read_oc
            else
                modflow.nOutputTimes=modflow.nOutputTimes+1
                read(instruction,*) OutputTimes(modflow.nOutputTimes)
                call Msg(TAB//instruction)
            end if  
        end do read_oc
        
        allocate(modflow.OutputTimes(modflow.nOutputTimes),stat=ialloc)
        call AllocChk(ialloc,'Output time array')
        
        modflow.OutputTimes(:modflow.nOutputTimes)=OutputTimes(:modflow.nOutputTimes)
        
        call Msg(' ')
        call Msg(TAB//'   #     Output time')
        call Msg(TAB//'--------------------')
        do i=1,modflow.nOutputTimes
            write(TmpSTR,'(i4,2x,g15.5,a)') i, modflow.OutputTimes(i),'     '//TRIM(modflow.STR_TimeUnit)
            call Msg(TAB//trim(TmpSTR))
        end do
    
        return
        
    end subroutine GenOCFile
    
    !-------------------------------------------------------------
    subroutine GrowKeyWordArray(Modflow,ndim) !--- during run if necessary 
        type (ModflowProject) Modflow
	    real, parameter :: nf_mult=2
	    integer :: ndim_new
	    integer :: ndim,i
	    character(MAX_STR), allocatable :: KeyWord_tmp(:) 

	    ndim_new=nint(ndim*nf_mult)
        write(*,*) 'ndim_new ', ndim_new

	    allocate(Keyword_tmp(ndim_new), stat=ialloc)
	    call AllocChk(ialloc,'allocate Keyword_tmp arrays')
	    Keyword_tmp(:)=char(0)

	    ! copy current data
	    do i=1,ndim
		    Keyword_tmp(i)	=	Modflow.Keyword(i)
	    end do

	    ! destroy arrays
	    deallocate(Modflow.Keyword)
	    ! reallocate
	    allocate(Modflow.Keyword(ndim_new), stat=ialloc)
	    call AllocChk(ialloc,'reallocate Modflow.Keyword arrays')
	    Modflow.Keyword(:)=char(0)

	    ! copy current data
	    do i=1,ndim
		    Modflow.Keyword(i)	=	Keyword_tmp(i)	
	    end do

	    ndim=ndim_new
	    
	    deallocate(Keyword_tmp)

    end subroutine GrowKeyWordArray

    !-------------------------------------------------------------
    subroutine GWF_IBOUNDv2_ToTecplot(Modflow)
        implicit none
        type (ModflowProject) Modflow

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i

       
        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.GWF.IBOUNDv2.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow IBOUND file Outputs "'

        write(FNum,'(a)') 'variables="X","Y","Z","IBOUND"'
        
        write(FNum,'(a)')'ZONE t="GWF IBOUND v2" '

        write(FNum,'(a)') '# x, y, z, ibound'
        do i=1, Modflow.GWF.nCells
            write(FNum,'(5e20.12)') Modflow.GWF.xcell(i),Modflow.GWF.ycell(i),Modflow.GWF.zcell(i),IBOUND(i)
        end do
   
        
        call FreeUnit(FNum)

    end subroutine GWF_IBOUNDv2_ToTecplot

    !-------------------------------------------------------------
    subroutine GWFToTecplot(Modflow,TMPLT_GWF)
        implicit none
        type(ModflowProject) Modflow
        type(TecplotDomain) TMPLT_GWF

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j

        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(modflow.GWF.name)//'.tecplot.dat'
        
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(TMPLT_GWF.name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","'//trim(modflow.GWF.name)//' Layer","'//trim(modflow.GWF.name)//' Zone",'
        nVar=5
            
        if(allocated(Modflow.GWF.Top)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Cell Top",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.GWF.Bottom)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Cell Bottom",'
            nVar=nVar+1
        end if

        if(allocated(Modflow.GWF.Kh)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Kh",'
            nVar=nVar+1
        end if

        if(allocated(Modflow.GWF.Kv)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Kv",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.GWF.Ss)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Ss",'
            nVar=nVar+1
        end if

        if(allocated(Modflow.GWF.Sy)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Sy",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.GWF.Alpha)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Alpha",'
            nVar=nVar+1
        end if
        if(allocated(Modflow.GWF.Beta)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Beta",'
            nVar=nVar+1
        end if
        if(allocated(Modflow.GWF.Sr)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Sr",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.GWF.Brooks)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Brooks",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.GWF.StartingHeads)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.GWF.name)//' Initial head",'
            nVar=nVar+1
        end if

        write(FNum,'(a)') trim(VarSTR)
          
        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(modflow.GWF.name)//'"  ,N=',TMPLT_GWF.nNodes,', E=',TMPLT_GWF.nElements,&
        ', datapacking=block, zonetype='//trim(TMPLT_GWF.elementtype)
        
        if(modflow.NodalControlVolume) then
            write(FNum,'(a)') trim(ZoneSTR) !//&
                !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
        else
            CellCenteredSTR=', VARLOCATION=([4'
            if(nVar.ge.5) then
                do j=5,nVar
                    write(str2,'(i2)') j
                    CellCenteredSTR=trim(CellCenteredSTR)//','//str2
                end do
            end if
            CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'

            write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)  !//&
                !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
        end if
        
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (TMPLT_GWF.x(i),i=1,TMPLT_GWF.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (TMPLT_GWF.y(i),i=1,TMPLT_GWF.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (TMPLT_GWF.z(i),i=1,TMPLT_GWF.nNodes)
        
            
            write(FNum,'(a)') '# layer'
            write(FNum,'(5i8)') (Modflow.GWF.iLayer(i),i=1,Modflow.GWF.nCells)
            write(FNum,'(a)') '# zone'
            write(FNum,'(5i8)') (Modflow.GWF.iZone(i),i=1,Modflow.GWF.nCells)
            write(FNum,'(a)') '# cell top'
            write(FNum,'(5e20.12)') (Modflow.GWF.Top(i),i=1,Modflow.GWF.nCells)
            write(FNum,'(a)') '# cell bottom'
            write(FNum,'(5e20.12)') (Modflow.GWF.Bottom(i),i=1,Modflow.GWF.nCells)
            
            if(allocated(Modflow.GWF.Kh)) then
                write(FNum,'(a)') '# Kh'
                write(FNum,'(5e20.12)') (Modflow.GWF.Kh(i),i=1,Modflow.GWF.nCells)
            end if
            
            if(allocated(Modflow.GWF.Kv)) then
                write(FNum,'(a)') '# Kv'
                write(FNum,'(5e20.12)') (Modflow.GWF.Kv(i),i=1,Modflow.GWF.nCells)
            end if
            
            if(allocated(Modflow.GWF.Ss)) then
                write(FNum,'(a)') '# Ss'
                write(FNum,'(5e20.12)') (Modflow.GWF.Ss(i),i=1,Modflow.GWF.nCells)
            end if

            
            if(allocated(Modflow.GWF.Sy)) then
                write(FNum,'(a)') '# Sy'
                write(FNum,'(5e20.12)') (Modflow.GWF.Sy(i),i=1,Modflow.GWF.nCells)
            end if

            
            if(allocated(Modflow.GWF.Alpha)) then
                write(FNum,'(a)') '# Alpha'
                write(FNum,'(5e20.12)') (Modflow.GWF.Alpha(i),i=1,Modflow.GWF.nCells)
            end if

            
            if(allocated(Modflow.GWF.Beta)) then
                write(FNum,'(a)') '# Beta'
                write(FNum,'(5e20.12)') (Modflow.GWF.Beta(i),i=1,Modflow.GWF.nCells)
            end if

            
            if(allocated(Modflow.GWF.Sr)) then
                write(FNum,'(a)') '# Sr'
                write(FNum,'(5e20.12)') (Modflow.GWF.Sr(i),i=1,Modflow.GWF.nCells)
            end if

            
            if(allocated(Modflow.GWF.Brooks)) then
                write(FNum,'(a)') '# Brooks'
                write(FNum,'(5e20.12)') (Modflow.GWF.Brooks(i),i=1,Modflow.GWF.nCells)
            end if

            if(allocated(Modflow.GWF.StartingHeads)) then
                write(FNum,'(a)') '# Initial head'
                write(FNum,'(5e20.12)') (Modflow.GWF.StartingHeads(i),i=1,Modflow.GWF.nCells)
            end if

        
        
        do i=1,TMPLT_GWF.nElements
            if(TMPLT_GWF.nNodesPerElement==6) then ! 6-node prism, repeat nodes 3 and 6 for 8-node tecplot type febrick
                write(FNum,'(8i8)') (TMPLT_GWF.iNode(j,i),j=1,3), TMPLT_GWF.iNode(3,i),(TMPLT_GWF.iNode(j,i),j=4,6), TMPLT_GWF.iNode(6,i) 
            else if(TMPLT_GWF.nNodesPerElement==8) then ! 8-node brick 
                write(FNum,'(8i8)') (TMPLT_GWF.iNode(j,i),j=1,8) 
            else
                write(TmpSTR,'(i2)') TMPLT_GWF.nNodesPerElement
                call ErrMsg(trim(TMPLT_GWF.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if
        end do
       
        call FreeUnit(FNum)
        
    end subroutine GWFToTecplot

    !-------------------------------------------------------------
    subroutine HGSToModflowStructure(FNumMUT, Modflow, TMPLT_GWF, TMPLT_SWF)
        implicit none
        

        integer :: FNumMUT
        type (HGSProject) hgs
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT_GWF
        type (TecplotDomain) TMPLT_SWF
       
        ! read prefix for HGS project
        read(FNumMUT,'(a)') hgs.Prefix
		call LwrCse(hgs.Prefix)
        call Msg('HGS model prefix: '//hgs.Prefix)

        
        call HGS_GetMeshComponents(FNumMUT, hgs)
        
        ! Flip HGS from bottom-up to top-down node and element numbering  
        call FlipHGSNumsBottomToTop(hgs)
        call HGS_ToTecplot(FNumMUT, hgs)
        
        ! Porous media aka GWF data
        if(hgs.mesh.nn == 0) then  ! something is wrong, no pm domain present
            call ErrMsg('*** HGS number of nodes is zero, no PM domain present')
        end if
        
        select case (hgs.mesh.nln)
        case (4)
            call ErrMsg('HGS 4-node tetrahedral elements not supported')
        case (6)
            call Msg('*** This HGS mesh has 6-node prism elements')
            TMPLT_GWF.ElementType='febrick'
        case (8)
            call Msg('*** This HGS mesh has 8-node brick elements')
            TMPLT_GWF.ElementType='febrick'
        case default
            write(TMPStr,'(i2)') hgs.mesh.nln
            call ErrMsg(trim(TmpSTR)//'-node HGS GWF elements not supported')
        end select
            
        ! Overland flow aka SWF data
        if(hgs.mesh.nolfe>0) then  ! olf domain present
            !call AddSWFFiles(FNumMUT, Modflow)
            select case (hgs.mesh.olfnln)
            case (4)
                if(hgs.mesh.inolf(4,1)==0) then
                    call Msg('*** This HGS mesh has 3-node triangular elements (stored as quads with 4th entry as zero')
                    TMPLT_SWF.ElementType='fequadrilateral'
                else
                    call Msg('*** This HGS mesh has 4-node quadrilateral elements stored as quads with 4th entry as zero')
                    TMPLT_SWF.ElementType='fequadrilateral'
                end if
            case default
                write(TMPStr,'(i2)') hgs.mesh.olfnln
                call ErrMsg(trim(TmpSTR)//'-node HGS OLF elements not supported')
            end select
            
            if(modflow.NodalControlVolume) then
                call ErrMsg('call HgsOlfToNodeCenteredModflow(hgs,modflow)')
            else ! MeshCentered
                call ErrMsg('!call HgsOlfToMeshCenteredModflow(hgs,TMPLT_SWF)')
            end if
           
            call DisplayDomainAttributes(TMPLT_SWF)
            !call FindNeighbours(Modflow,modflow.SWF)
            call SWFToTecplot(Modflow,TMPLT_SWF)
            call IaJa_FromTecplot(TMPLT_SWF)
            !call WriteSWFFiles(Modflow)
        end if
        
            
        if(Modflow.NodalControlVolume) then
            call ErrMsg('!call HgsPmToNodeCenteredModflow(hgs,modflow)')
        else ! MeshCentered
            call ErrMsg('!call HgsPmToMeshCenteredModflow(hgs,TMPLT_GWF)')
        end if
        
        call DisplayDomainAttributes(TMPLT_GWF)
        
        call GWFToTecplot(Modflow,TMPLT_GWF)
        call IaJa_FromTecplot(TMPLT_GWF)
        !call WriteGWFFiles(Modflow)
        
        ! Fracture element data
        if(hgs.mesh.nef>0) then  ! fracture domain present
            call Msg('*** HGS fracture domain present but not supported')
        end if
        
    end subroutine HGSToModflowStructure
    
    !-------------------------------------------------------------

  !  !-------------------------------------------------------------
  !  subroutine HgsPmToMeshCenteredModflow(Hgs,TMPLT_GWF)
  !      implicit none
  !      type (HgsProject) Hgs
  !      type (TecplotDomain) TMPLT_GWF
  !      integer :: i, j
  !      
  !     
  !      TMPLT_GWF.meshtype='UNSTRUCTURED'
  !      
  !      if(.not. allocated(TMPLT_GWF.x)) then
  !          ! These HGS node coordinates are used in the tecplot output file 
  !          TMPLT_GWF.nNodes=hgs.mesh.nn  
  !          allocate(TMPLT_GWF.x(TMPLT_GWF.nNodes),TMPLT_GWF.y(TMPLT_GWF.nNodes),TMPLT_GWF.z(TMPLT_GWF.nNodes), stat=ialloc)
  !          call AllocChk(ialloc,'GWF node coordinate arrays')
  !          TMPLT_GWF.x(:)= hgs.mesh.x(:)
  !          TMPLT_GWF.y(:)= hgs.mesh.y(:)
  !          TMPLT_GWF.z(:)= hgs.mesh.z(:)
  !      end if
  !      
  !      
  !      TMPLT_GWF.nLayers=hgs.nsheet-1
  !      !TMPLT_GWF.iz=0
  !      !TMPLT_GWF.nodelay=hgs.ne2d   ! number of modflow cells per layer
  !
  !      
  !      TMPLT_GWF.nNodesPerElement=hgs.mesh.nln
  !      TMPLT_GWF.nElements=hgs.mesh.ne
  !      
  !      ! Just define these for now
  !      !TMPLT_GWF.ic=0
  !      
  !      !
  !      !allocate(TMPLT_GWF.xCell(TMPLT_GWF.nCells),TMPLT_GWF.yCell(TMPLT_GWF.nCells),TMPLT_GWF.zCell(TMPLT_GWF.nCells),TMPLT_GWF.iLayer(TMPLT_GWF.nCells),stat=ialloc)
  !      !call AllocChk(ialloc,'GWF cell coordinate and layer arrays')
  !      
  !      !allocate(hgs.mesh.in(hgs.mesh.nln,hgs.mesh.ne),stat=ialloc)
  !      allocate(TMPLT_GWF.iNode(TMPLT_GWF.nNodesPerElement,TMPLT_GWF.nElements),stat=ialloc)
  !      call AllocChk(ialloc,'GWF iNode arrays')
  !      TMPLT_GWF.iNode(:,:) = hgs.mesh.in(:,:) 
  !      
  !      !! Layer has quasi-3D confining bed below if non-zero
  !      !allocate(TMPLT_GWF.Laybcd(TMPLT_GWF.nLayers),stat=ialloc)
  !      !call AllocChk(ialloc,'GWF.Laybcd array')
  !      !TMPLT_GWF.Laybcd(:)=0
  !
  !      
  !      !!read(itmp) ((hgs.mesh.in(j,i),j=1,hgs.mesh.nln),i=1,hgs.mesh.ne)
  !      !read(itmp) ((TMPLT_GWF.iNode(j,i),j=1,TMPLT_GWF.nNodesPerElement),i=1,TMPLT_GWF.nCells)
  !      
  !      !if(modflow.InnerCircles) then
  !      !    ! Use the SWF cel xy coords for the GWF cell but z as the centroid of the iNode array z coordinates
  !      !    do i=1,TMPLT_GWF.nCells
  !      !        zc=0.0
  !      !        do j=1,TMPLT_GWF.nNodesPerElement
  !      !            zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
  !      !        end do
  !      !        TMPLT_GWF.xCell(i)=Modflow.SWF.xCell(myMOD(i,Modflow.SWF.nCells))
  !      !        TMPLT_GWF.yCell(i)=Modflow.SWF.yCell(myMOD(i,Modflow.SWF.nCells))
  !      !        TMPLT_GWF.zCell(i)=zc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.iLayer(i)=1
  !      !    end do
  !      !    
  !      !else
  !      !    ! Generate the modflow cell coordinates as the centroid of the iNode array coordinates
  !      !    do i=1,TMPLT_GWF.nCells
  !      !        xc=0.0
  !      !        yc=0.0
  !      !        zc=0.0
  !      !        do j=1,TMPLT_GWF.nNodesPerElement
  !      !            xc=xc+TMPLT_GWF.x(TMPLT_GWF.iNode(j,i))
  !      !            yc=yc+TMPLT_GWF.y(TMPLT_GWF.iNode(j,i))
  !      !            zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
  !      !        end do
  !      !        TMPLT_GWF.xCell(i)=xc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.yCell(i)=yc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.zCell(i)=zc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.iLayer(i)=1
  !      !    end do
  !      !end if
  !      
  !      allocate(TMPLT_GWF.iZone(TMPLT_GWF.nElements),stat=ialloc)
  !      call AllocChk(ialloc,'Read 3d Element property array')
  !      TMPLT_GWF.iZone = 0 ! automatic initialization
  !      TMPLT_GWF.iZone(:)=hgs.iprop(:)
	 !   
  !      call freeunit(itmp)
  !  end subroutine HgsPmToMeshCenteredModflow
  !
  !  !-------------------------------------------------------------
  !  subroutine HgsOlfToMeshCenteredModflow(hgs,TMPLT_SWF)
  !      implicit none
  !      type (HgsProject) Hgs
  !      type (TecplotDomain) TMPLT_SWF
  !
  !      integer :: i,j,i1
  !      real*8 :: x(3),y(3)
  !      real*8 :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)
  !      
  !      integer, allocatable :: TMPArray(:,:)
  !      
  !     TMPLT_SWF.meshtype='UNSTRUCTURED'
  !      
  !      TMPLT_SWF.nNodes=hgs.nnolf
  !
  !      allocate(TMPLT_SWF.x(TMPLT_SWF.nNodes),TMPLT_SWF.y(TMPLT_SWF.nNodes),TMPLT_SWF.z(TMPLT_SWF.nNodes), stat=ialloc)
  !      call AllocChk(ialloc,'SWF node coordinate arrays')
  !      TMPLT_SWF.x = 0 ! automatic initialization
  !      TMPLT_SWF.y = 0 ! automatic initialization
  !      TMPLT_SWF.z = 0 ! automatic initialization
  !      
  !      
  !      allocate(TMPLT_SWF.gwf_nn_from_swf_nn(TMPLT_SWF.nNodes),&
  !               TMPLT_SWF.swf_nn_from_gwf_nn(TMPLT_GWF.nNodes), stat=ialloc)
  !      call AllocChk(ialloc,'SWF link_pm arrays')
  !      TMPLT_SWF.gwf_nn_from_swf_nn(:) = hgs.link_Olf2pm(:)
  !      TMPLT_SWF.swf_nn_from_gwf_nn(:) = hgs.link_pm2Olf(:)
  !
  !      if(.not. allocated(Modflow.GWF.x)) then
  !          ! These HGS node coordinates are used in the tecplot output file 
  !          Modflow.GWF.nNodes=hgs.mesh.nn  
  !          allocate(Modflow.GWF.x(Modflow.GWF.nNodes),Modflow.GWF.y(Modflow.GWF.nNodes),Modflow.GWF.z(Modflow.GWF.nNodes), stat=ialloc)
  !          call AllocChk(ialloc,'GWF node coordinate arrays')
  !          Modflow.GWF.x(:)= hgs.mesh.x(:)
  !          Modflow.GWF.y(:)= hgs.mesh.y(:)
  !          Modflow.GWF.z(:)= hgs.mesh.z(:)
  !      end if
  !
  !      do i=1,TMPLT_SWF.nNodes
  !          TMPLT_SWF.x(i)=Modflow.GWF.x(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !          TMPLT_SWF.y(i)=Modflow.GWF.y(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !          TMPLT_SWF.z(i)=Modflow.GWF.z(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !      end do
  !      
  !      
  !      TMPLT_SWF.nNodesPerElement = hgs.mesh.OlfNLN
  !      
  !      TMPLT_SWF.nElements = hgs.mesh.nolfe
  !      
  !      allocate(TMPLT_SWF.xCell(TMPLT_SWF.nCells),TMPLT_SWF.yCell(TMPLT_SWF.nCells),TMPLT_SWF.zCell(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF cell coordinate arrays')
  !      allocate(TMPLT_SWF.iLayer(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF layer array')
  !      
		!!allocate(hgs.mesh.inolf(hgs.mesh.OlfNLN,hgs.mesh.nolfe), &
		!!	hgs.iolf_id_elem(hgs.mesh.nolfe), &
		!!	hgs.iolf_3d_elem_map(hgs.mesh.nolfe), &
		!!	stat=ialloc)
		!!call AllocChk(ialloc,'Overland element arrays')
  !      allocate(TMPLT_SWF.iNode(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF iNode arrays')
  !      TMPLT_SWF.iNode(:,:) = hgs.mesh.inolf(:,:)
  !      
  !      ! These numbers represent the pm node number that is coincicent to the olf node
  !      allocate(TMPArray(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF TMPArray')
  !      TMPArray=0
  !      do i=1,TMPLT_SWF.nCells
  !          do j=1,TMPLT_SWF.nNodesPerElement
  !              ! HGS assumes 4 nodes for OLF elements but if triangles 4th value is zero so check
  !              if(TMPLT_SWF.iNode(j,i) > 0) then
  !                  ! The conversion to SWF nn from GWF nn is a global SWF nn so subtract total number of GWF nodes
  !                  TMPArray(j,i)=TMPLT_SWF.swf_nn_from_gwf_nn(TMPLT_SWF.iNode(j,i))-Modflow.GWF.nNodes
  !              end if
  !          end do
  !      end do
  !      TMPLT_SWF.iNode=TMPArray
  !      deallocate(TMPArray)
  !
  !      
  !      ! Generate the modflow cell coordinates 
  !      ! 
  !      if(TMPLT_SWF.iNode(4,1) > 0) then ! 4-node rectangles, use centroid of the iNode array coordinates
  !          do i=1,TMPLT_SWF.nCells
  !              xc=0.0
  !              yc=0.0
  !              zc=0.0
  !              do j=1,TMPLT_SWF.nNodesPerElement
  !                  xc=xc+TMPLT_SWF.x(TMPLT_SWF.iNode(j,i))
  !                  yc=yc+TMPLT_SWF.y(TMPLT_SWF.iNode(j,i))
  !                  zc=zc+TMPLT_SWF.z(TMPLT_SWF.iNode(j,i))
  !              end do
  !              TMPLT_SWF.xCell(i)=xc/4
  !              TMPLT_SWF.yCell(i)=yc/4
  !              TMPLT_SWF.zCell(i)=zc/4
  !              TMPLT_SWF.iLayer(i)=1
  !          end do
  !      else ! triangle with third node repeated, use circumcircle approach
  !          
  !          !allocate(BadTri(TMPLT_SWF.nCells),stat=ialloc)
  !          !call AllocChk(ialloc,'BadTri Array')
  !          !BadTri(:)=0
  !
  !          do i=1,TMPLT_SWF.nCells
  !              ! xc and yc from circumcircles
  !              do j=1,3
  !                  x(j)=TMPLT_SWF.x(TMPLT_SWF.iNode(j,i))
  !                  y(j)=TMPLT_SWF.y(TMPLT_SWF.iNode(j,i))
  !              end do
  !              !call OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
  !              !if(bad_triangle) then
  !              !    !write(*,*) 'element ',i,' bad triangle for circumcenter'
  !              !    call InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
  !              !    BadTri(i)=1
  !              !end if
  !              !call OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
  !              !if(bad_triangle) then
  !              !    !write(*,*) 'element ',i,' bad triangle for circumcenter'
  !                  call InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
  !              !    BadTri(i)=1
  !              !end if
  !              
  !              ! zc from centroid of the iNode array coordinates
  !              zc=0.0
  !              do j=1,3
  !                  zc=zc+TMPLT_SWF.z(TMPLT_SWF.iNode(j,i))
  !              end do
  !              
  !              TMPLT_SWF.xCell(i)=xc
  !              TMPLT_SWF.yCell(i)=yc
  !              TMPLT_SWF.zCell(i)=zc/3
  !              TMPLT_SWF.iLayer(i)=1
  !          end do
  !      end if
  !      
  !      Modflow.InnerCircles=.true.
  !      
  !      allocate(TMPLT_SWF.iZone(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'Read SWF cell property array')
  !      TMPLT_SWF.iZone(:) = hgs.iolf_id_elem(:i)
  !      
  !      
  !      
  !      
  !  end subroutine HgsOlfToMeshCenteredModflow
 
    !-------------------------------------------------------------
    subroutine IaJa_FromTecplot(domain)
        implicit none
        type(TecplotDomain) domain

        integer :: Fnum
        character(MAX_STR) :: FName
        character(MAX_STR) :: FNameTecplotDat
        integer :: i, j, nFaceNeighborConnections, nNodes, nElements, iNja 
        character(4000) :: output_line
        character(4000) :: var_line
        
        integer :: iConn
        
        FNameTecplotDat='scratcho.'//trim(domain.name)//'.tecplot.dat'
        
        call ModflowDomainGridToTecplot(FNameTecplotDat,domain)

        nFaceNeighborConnections=0
        
        call Msg(' ')
        call Msg('  Generating IA/JA and cell connection arrays for domain '//trim(domain.name)//'...')
            
        call OpenAscii(FNum,FNameTecplotDat)
        do 
            read(FNum,'(a)',iostat=status) line
            if(status/=0) exit
            
            if(index(line,'variables=') > 0) then
                l1=index(line,'variables=')+10
                var_line=line(l1:)
               
            else if(index(line,'zonetype=') > 0) then
                l1=index(line,'zonetype=')+9
                read(line(l1:),*) output_line
                domain.ElementType=trim(output_line)
                exit
            end if
                
        end do
        call freeunit(FNum)
        
        FName=' scratcho.'//trim(domain.name)//'.mcr'
        call OpenAscii(FNum,FName)
        write(FNum,'(a)') '#!MC 1410'
        write(FNum,'(a)') '$!ReadDataSet  "'//trim(FNameTecplotDat)//'"'
        write(FNum,'(a)') '  ReadDataOption = New'
        write(FNum,'(a)') '  ResetStyle = No'
        write(FNum,'(a)') '  VarLoadMode = ByName'
        write(FNum,'(a)') '  AssignStrandIDs = Yes'
        write(FNum,'(a)') '  VarNameList =  '''//trim(var_line)//''''
        write(FNum,'(a)') '$!WriteDataSet  "'//' scratcho.'//trim(domain.name)//'.neighbours.dat"'
        write(FNum,'(a)') '  IncludeText = No'
        write(FNum,'(a)') '  IncludeGeom = No'
        write(FNum,'(a)') '  IncludeCustomLabels = No'
        write(FNum,'(a)') '  IncludeDataShareLinkage = Yes'
        write(FNum,'(a)') '  IncludeAutoGenFaceNeighbors = Yes'
        ! Must write one variable so write X
        write(FNum,'(a)') '  VarPositionList =  [1]'               
        write(FNum,'(a)') '  Binary = No'
        write(FNum,'(a)') '  UsePointFormat = Yes'
        write(FNum,'(a)') '  Precision = 9'
        write(FNum,'(a)') '  TecplotVersionToWrite = TecplotCurrent'
        call FreeUnit(FNum)
        
        
        FName=' scratcho.'//trim(domain.name)//'.bat'
        call OpenAscii(FNum,FName)
        !write(FNum,'(a)') 'echo'
        write(FNum,'(a)') 'tec360 -b -p  scratcho.'//trim(domain.name)//'.mcr'
        write(FNum,'(a)') ' '
        call FreeUnit(FNum)
        
        CmdLine=' scratcho.'//trim(domain.name)//'.bat'
        CALL execute_command_line(trim(CmdLine)) 
        
        ! get cell neighbours (ia, ja structure) from tecplot output file
        FNameTecplotDat=' scratcho.'//trim(domain.name)//'.neighbours.dat'
        call OpenAscii(FNum,FNameTecplotDat)
        do 
            read(FNum,'(a)',iostat=status) line
            if(status/=0) exit
            
            if(index(line,'FACENEIGHBORCONNECTIONS=') > 0) then
                l1=index(line,'=')+1
                read(line(l1:),*) nFaceNeighborConnections
            else if(index(line,'Nodes=') > 0) then
                l1=index(line,'Nodes=')+6
                read(line(l1:),*) nNodes
                l1=index(line,'Elements=')+9
                read(line(l1:),*) nElements
            else if(index(line,'DT=(SINGLE )') > 0) then
                ! read x coordinate lines
                do i=1,nNodes
                    read(FNum,'(a)') line
                end do 
                ! read node list lines
                do i=1,nElements
                    read(FNum,'(a)') line
                end do 
                ! read and store cell connection information
                allocate(domain.Element(nFaceNeighborConnections),&
                        domain.Face(nFaceNeighborConnections),&
                        domain.Neighbour(nFaceNeighborConnections),stat=ialloc)
                call AllocChk(ialloc,'Element neighbour arrays')
                do i=1,nFaceNeighborConnections
                    read(FNum,*) domain.Element(i),domain.Face(i),domain.Neighbour(i)
                end do
                ! form ia, ja
                
                domain.njag=nFaceNeighborConnections+nElements
                allocate(domain.ia(nElements),&
                        domain.ja(domain.njag),stat=ialloc)
                call AllocChk(ialloc,'Element neighbour ia ja arrays')
                domain.ia(:)=1
                do i=1,nFaceNeighborConnections
                    domain.ia(domain.Element(i))=domain.ia(domain.Element(i))+1
                end do

                domain.ja(:)=0
                iNja=0
                iConn=0
                do i=1,nElements
                    iNja=iNja+1
                    domain.ja(iNja)=i
                    do j=2,domain.ia(i)
                        iConn=iConn+1
                        iNja=iNja+1
                        domain.ja(iNja)=domain.neighbour(iConn)
                    end do
                end do
                exit
                
            end if
        end do
        call freeunit(FNum)
            
        return
    end subroutine IaJa_FromTecplot

    !----------------------------------------------------------------------
    subroutine InitialHeadFunctionOfZtoGWF(FNumMUT,domain)
        implicit none
        integer :: FNumMUT
        type(ModflowDomain) domain

        integer :: i, j
	    integer :: npairs
	    real(dr) :: t
                
        character(256) :: instruction

	    real(dr) :: zp(1000)
	    real(dr) :: hp(1000)
	    zp(:) = 0
	    hp(:) = 0


        call Msg(TAB//'                Z              Head')

	    npairs=0
	    read_zhead_pairs:do
		    read(FNumMUT,'(a)',iostat=status) instruction
		    if(status /= 0) exit

		    len=len_trim(instruction)
            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg(TAB//'end gwf initial head function of z')
                exit read_zhead_pairs
		    else
			    npairs=npairs+1
			    read(instruction,*,iostat=status) zp(npairs),hp(npairs)

			    if(npairs > 1) then
			        if(zp(npairs) <= zp(npairs-1)) then
				        call ErrMsg('Z values must be entered in ascending order')
				    endif
			    endif

			    if(status /= 0) then
				    call ErrMsg('Bad z-head pair')
                endif
                
                write(TmpSTR,'(i8,2x,2g15.5)') npairs,zp(npairs),hp(npairs)
                call Msg(TAB//trim(TmpSTR))

		    endif
	    end do read_zhead_pairs

        do i=1,domain.nCells
		    do j=1,npairs-1
			    if(domain.zCell(i) >= zp(j) .and. domain.zCell(i) <= zp(j+1)) then  ! interpolate
	                t=(domain.zCell(i)-zp(j))/(zp(j+1)-zp(j))
				    domain.StartingHeads(i)=(1.0-t)*hp(j)+t*hp(j+1)
			    end if
		    end do
        end do
        
        call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))


    end subroutine InitialHeadFunctionOfZtoGWF
   
    !-------------------------------------------------------------
    subroutine InitializeModflowFiles(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
               
        call Msg('  ')
        call Msg('  Initialize Modflow project files with prefix: '//Modflow.Prefix)
        
        ! Initialize default units
        Modflow.STR_LengthUnit='METERS'
        Modflow.STR_TimeUnit='SECONDS'
        
        ! Initialize NAM fil
        Modflow.FNameNAM=trim(Modflow.Prefix)//'.nam'
        call OpenAscii(Modflow.iNAM,Modflow.FNameNAM)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameNAM))
        write(Modflow.iNAM,'(a,1pg10.1)') '# MODFLOW-USG NAM file written by Modflow-User-Tools version ',MUTVersion

        ! Get unit number for LST file and write data to NAM
        Modflow.FNameLIST=trim(Modflow.Prefix)//'.lst'
        call getunit(Modflow.iLIST)
        write(Modflow.iNAM,'(a,i4,a)') 'LIST ',Modflow.iLIST,' '//trim(Modflow.FNameLIST)

        ! Initialize BAS6 file and write data to NAM
        Modflow.FNameBAS6=trim(Modflow.Prefix)//'.bas'
        call OpenAscii(Modflow.iBAS6,Modflow.FNameBAS6)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameBAS6))
        write(Modflow.iNAM,'(a,i4,a)') 'BAS6 ',Modflow.iBAS6,' '//trim(Modflow.FNameBAS6)
        write(Modflow.iBAS6,'(a)') '# MODFLOW-USGs Basic Package'
        write(Modflow.iBAS6,'(a,1pg10.1)') '# MODFLOW-USG BAS6 file written by Modflow-User-Tools version ',MUTVersion

        ! Initialize SMS file and write data to NAM
        Modflow.FNameSMS=trim(Modflow.Prefix)//'.sms'
        call OpenAscii(Modflow.iSMS,Modflow.FNameSMS)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameSMS))
        write(Modflow.iNAM,'(a,i4,a)') 'SMS  ',Modflow.iSMS,' '//trim(Modflow.FNameSMS)
        write(Modflow.iSMS,'(a,1pg10.1)') '# MODFLOW-USG SMS file written by Modflow-User-Tools version ',MUTVersion

        ! Initialize OC file and write data to NAM
        Modflow.FNameOC=trim(Modflow.Prefix)//'.oc'
        call OpenAscii(Modflow.iOC,Modflow.FNameOC)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameOC))
        write(Modflow.iNAM,'(a,i4,a)') 'OC   ',Modflow.iOC,' '//trim(Modflow.FNameOC)
        write(Modflow.iOC,'(a,1pg10.1)') '# MODFLOW-USG OC file written by Modflow-User-Tools version ',MUTVersion
        
        
        ! Initialize LPF file and write data to NAM
        Modflow.FNameLPF=trim(Modflow.Prefix)//'.lpf'
        call OpenAscii(Modflow.iLPF,Modflow.FNameLPF)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameLPF))
        write(Modflow.iNAM,'(a,i4,a)') 'LPF  ',Modflow.iLPF,' '//trim(Modflow.FNameLPF)
        write(Modflow.iLPF,'(a,1pg10.1)') '# MODFLOW-USG Layer Property Flow (LPF) Package written by Modflow-User-Tools version ',MUTVersion
        
        ! Initialize GSF file and write data to NAM
        Modflow.FNameGSF=trim(Modflow.Prefix)//'.gwf.gsf'
        call OpenAscii(Modflow.iGSF,Modflow.FNameGSF)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameGSF))
        write(Modflow.iGSF,'(a,1pg10.1)') '# MODFLOW-USG Grid Specification File (GSF) written by Modflow-User-Tools version ',MUTVersion
        
        ! Initialize SWF_GSF file and write data to NAM
        Modflow.FNameSWF_GSF=trim(Modflow.Prefix)//'.swf.gsf'
        call OpenAscii(Modflow.iSWF_GSF,Modflow.FNameSWF_GSF)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameSWF_GSF))
        write(Modflow.iSWF_GSF,'(a,1pg10.1)') '# MODFLOW-USG Grid Specification File (SWF_GSF) Package written by Modflow-User-Tools version ',MUTVersion

        ! Initialize CLN_GSF file and write data to NAM
        Modflow.FNameCLN_GSF=trim(Modflow.Prefix)//'.cln.gsf'
        call OpenAscii(Modflow.iCLN_GSF,Modflow.FNameCLN_GSF)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameCLN_GSF))
        write(Modflow.iCLN_GSF,'(a,1pg10.1)') '# MODFLOW-USG Grid Specification File (CLN_GSF) Package written by Modflow-User-Tools version ',MUTVersion
        
        ! Initialize DIS file and write data to NAM
        Modflow.FNameDISU=trim(Modflow.Prefix)//'.dis'
        call OpenAscii(Modflow.iDISU,Modflow.FNameDISU)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameDISU))
        write(Modflow.iNAM,'(a,i4,a)') 'DISU ',Modflow.iDISU,' '//trim(Modflow.FNameDISU)
        write(Modflow.iDISU,'(a,1pg10.1)') '# MODFLOW-USG DIS file written by Modflow-User-Tools version ',MUTVersion
        
        Modflow.GWF.FNameCBB=trim(Modflow.Prefix)//'.GWF.cbb'
        call getunit(Modflow.GWF.iCBB)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.GWF.iCBB,' '//trim(Modflow.GWF.FNameCBB)
        
        Modflow.GWF.FNameHDS=trim(Modflow.Prefix)//'.GWF.HDS'
        call getunit(Modflow.GWF.iHDS)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.GWF.iHDS,' '//trim(Modflow.GWF.FNameHDS)
        
        Modflow.GWF.FNameDDN=trim(Modflow.Prefix)//'.GWF.DDN'
        call getunit(Modflow.GWF.iDDN)
        write(Modflow.iNAM,'(a,i4,a)') 'DATA(BINARY) ',Modflow.GWF.iDDN,' '//trim(Modflow.GWF.FNameDDN)

        return
    end subroutine InitializeModflowFiles


    !----------------------------------------------------------------------
    subroutine MeshCentredCLNCellGeometry(Modflow, TMPLT_CLN,TMPLT)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_CLN
        integer :: i

        ! For modflow cell connection and area calculations
        
        ! Generate ia/ja from scratch
        call CLN_IaJaStructure(TMPLT_CLN)

        allocate(Modflow.CLN.ConnectionLength(TMPLT_CLN.njag),Modflow.CLN.PerpendicularArea(TMPLT_CLN.njag),stat=ialloc)
        call AllocChk(ialloc,'CLN Cell connection length, perpendicular area array')
        Modflow.CLN.ConnectionLength(:)=0.0d0
        Modflow.CLN.PerpendicularArea(:)=0.0d0

        Modflow.CLN.njag=TMPLT_CLN.njag
        
        allocate(Modflow.CLN.ia(TMPLT_CLN.nElements),Modflow.CLN.ja(TMPLT_CLN.njag),stat=ialloc)
        call AllocChk(ialloc,'CLN ia, ja arrays')
        Modflow.CLN.ia(:)=TMPLT_CLN.ia
        Modflow.CLN.ja(:)=TMPLT_CLN.ja

        ! Cell horizontal areas
        ! use existing TMPLT.CellArea (e.g. area of triangle)  
        allocate(Modflow.CLN.CellArea(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,'CLN cell horizontal area arrays')
        do i=1,Modflow.CLN.nCells
            Modflow.CLN.CellArea(i)=TMPLT.ElementArea(i)
        end do

        !! Cell connection length and perpendicular area arrays
        !! cRM  Are these used, or just FLENG and ??
        !iConn=0
        !iNbor=0
        !do i=1,modflow.CLN.nCells
        !    iConn=iConn+1
        !            
        !    do j=2,modflow.CLN.ia(i)
        !        iConn=iConn+1
        !        inBor=iNbor+1
        !        modflow.CLN.ConnectionLength(iConn)=TMPLT_CLN.Length(i)/2.0d0 
        !        modflow.CLN.PerpendicularArea(iConn)=TMPLT_CLN.DiameterOrWidth(i)   
        !    end do
        !end do
    
    end subroutine MeshCentredCLNCellGeometry

    !----------------------------------------------------------------------
    subroutine MeshCentredSWFCellGeometry(Modflow, TMPLT_SWF,TMPLT)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_SWF
        integer :: i, j

        ! For modflow cell connection and area calculations
        
        integer :: iConn, iNbor


        ! Generate ia/ja from face neighbour data calculated by Tecplot
        call IaJa_FromTecplot(TMPLT_SWF)

        allocate(Modflow.SWF.ConnectionLength(TMPLT_SWF.njag),Modflow.SWF.PerpendicularArea(TMPLT_SWF.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF Cell connection length, perpendicular area array')
        Modflow.SWF.ConnectionLength(:)=0.0d0
        Modflow.SWF.PerpendicularArea(:)=0.0d0

        Modflow.SWF.njag=TMPLT_SWF.njag
        
        allocate(Modflow.SWF.ia(TMPLT_SWF.nElements),Modflow.SWF.ja(TMPLT_SWF.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF ia, ja arrays')
        Modflow.SWF.ia(:)=TMPLT_SWF.ia
        Modflow.SWF.ja(:)=TMPLT_SWF.ja

        ! Cell horizontal areas
        ! use existing TMPLT.CellArea (e.g. area of triangle)  
        allocate(Modflow.SWF.CellArea(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'SWF cell horizontal area arrays')
        do i=1,Modflow.SWF.nCells
            Modflow.SWF.CellArea(i)=TMPLT.ElementArea(i)
        end do

        ! Cell connection length and perpendicular area arrays
        iConn=0
        iNbor=0
        do i=1,modflow.SWF.nCells
            iConn=iConn+1
                    
            do j=2,modflow.SWF.ia(i)
                iConn=iConn+1
                inBor=iNbor+1
                ! SWF neighbours always in adjacent column
                if(TMPLT.nNodesPerElement == 3) then
                    select case (TMPLT_SWF.face(iNbor))
                        case ( 1 )
                            modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(3,i)   
                        case ( 2 )
                            modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(2,i)   
                        case ( 3 )
                            modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(1,i)   
                        case ( 4 )  ! must be 8-node block
                            call ErrMsg('Need to create sideLength for 2D rectangle/3D block case')
                    end select
                    modflow.SWF.ConnectionLength(iConn)=TMPLT.rCircle(i)
                    modflow.SWF.PerpendicularArea(iConn)=modflow.SWF.PerpendicularArea(iConn)*1.0d0  ! assume thickness of 1?
                else if(TMPLT.nNodesPerElement == 4) then
                    select case (TMPLT_SWF.face(iNbor))
                    case ( 1 )
                        modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(1,i)   
                        modflow.SWF.ConnectionLength(iConn)=TMPLT.SideLength(1,i)/2.0d0
                    case ( 2 )
                        modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(2,i)   
                        modflow.SWF.ConnectionLength(iConn)=TMPLT.SideLength(2,i)/2.0d0
                    case ( 3 )
                        modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(3,i)   
                        modflow.SWF.ConnectionLength(iConn)=TMPLT.SideLength(3,i)/2.0d0
                    case ( 4 )
                        modflow.SWF.PerpendicularArea(iConn)=TMPLT.SideLength(4,i)   
                        modflow.SWF.ConnectionLength(iConn)=TMPLT.SideLength(4,i)/2.0d0
                    end select
                    modflow.SWF.PerpendicularArea(iConn)=modflow.SWF.PerpendicularArea(iConn)**1.0d0  ! assume thickness of 1?
                end if
            end do
        end do
    
    end subroutine MeshCentredSWFCellGeometry

    !-------------------------------------------------------------
    subroutine ModflowDomainGridToTecplot(FName,domain)
        implicit none
        type(TecplotDomain) domain

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j

        
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(domain.name)//'"'

        write(FNum,'(a)') 'variables="X","Y","Z"'
        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(domain.name)//'" ,N=',domain.nNodes,', E=',domain.nElements,', datapacking=block, zonetype='//trim(domain.elementtype)
        write(FNum,'(a)') trim(ZoneSTR)
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (domain.x(i),i=1,domain.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (domain.y(i),i=1,domain.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (domain.z(i),i=1,domain.nNodes)
        
        do i=1,domain.nElements
            if(domain.name == 'TMPLT_GWF') then
                if(domain.nNodesPerElement==6) then ! 6-node prism, repeat nodes 3 and 6 for 8-node tecplot type febrick
                    write(FNum,'(8i8)') (domain.iNode(j,i),j=1,3), domain.iNode(3,i),(domain.iNode(j,i),j=4,6), domain.iNode(6,i) 
                else if(domain.nNodesPerElement==8) then ! 8-node brick 
                    write(FNum,'(8i8)') (domain.iNode(j,i),j=1,8) 
                else
                    write(TmpSTR,'(i2)') domain.nNodesPerElement
                    call ErrMsg(trim(domain.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
                end if
            else if(domain.name == 'TMPLT_SWF' .or. domain.name == 'TMPLT') then
                if(domain.nNodesPerElement==3) then ! 3-node triangle, repeat node 3 for 4-node tecplot type fequadrilateral
                    write(FNum,'(8i8)') (domain.iNode(j,i),j=1,3) !, domain.iNode(3,i) 
                else if(domain.nNodesPerElement==4) then ! 4-node quadrilateral
                    if(domain.iNode(4,i) > 0) then
                        write(FNum,'(8i8)') (domain.iNode(j,i),j=1,4) 
                    else
                        write(FNum,'(8i8)') (domain.iNode(j,i),j=1,3), domain.iNode(3,i) 
                    end if
                else
                    write(TmpSTR,'(i2)') domain.nNodesPerElement
                    call ErrMsg(trim(domain.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
                end if
            else if(domain.name == 'TMPLT_CLN' .or. domain.name == 'TMPLT') then
                if(domain.nNodesPerElement==2) then ! 2-node line
                    write(FNum,'(8i8)') (domain.iNode(j,i),j=1,2) 
                else
                    write(TmpSTR,'(i2)') domain.nNodesPerElement
                    call ErrMsg(trim(domain.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
                end if
            end if
        end do

        call FreeUnit(FNum)
       
            
    end subroutine ModflowDomainGridToTecplot
    
    !-------------------------------------------------------------
    subroutine ModflowDomainScatterToTecplot(Modflow,domain)
        implicit none
        type (ModflowProject) Modflow
        type(ModflowDomain) domain
        
        integer :: i
        character(MAX_STR) :: FName
        
        If(allocated(domain.xCell)) then
            ! Write Modflow cell coordinates as tecplot scatter data
           
                FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'_CELLS.tecplot.dat'
            
            call OpenAscii(FNum,FName)
            call Msg('  ')
            call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
            write(FNum,'(a)') 'Title = " Modflow '//trim(domain.name)//' CELLS"'

            VarSTR='variables="X","Y","Z"'
            nVar=3
            
            write(FNum,'(a)') trim(VarSTR)
            write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nCells,', t="'//trim(domain.name)//' CELLS", datapacking=point'
        
            write(FNum,'(a)') trim(ZoneSTR)
            !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
            !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'

            do i=1,domain.nCells
                write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i)
            end do
            
            call FreeUnit(FNum)

            if(domain.nCHDCells > 0) then
                FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'_CHD.tecplot.dat'
            
                call OpenAscii(FNum,FName)
                call Msg('  ')
                call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
                write(FNum,'(a)') 'Title = " Modflow '//trim(domain.name)//' CHD"'

                VarSTR='variables="X","Y","Z","CHD"'
                nVar=3
            
                write(FNum,'(a)') trim(VarSTR)
            
                write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nCHDCells,', t="'//trim(domain.name)//' CHD", datapacking=point'
        
                write(FNum,'(a)') trim(ZoneSTR)
           
                do i=1,domain.nCells
                    if(bcheck(domain.Cell_is(i),ConstantHead)) write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i),&
                        domain.ConstantHead(i)
                end do
            
                call FreeUnit(FNum)
            end if
            
            if(allocated(domain.recharge)) then
                FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'_RCH.tecplot.dat'
            
                call OpenAscii(FNum,FName)
                call Msg('  ')
                call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
                write(FNum,'(a)') 'Title = " Modflow '//trim(domain.name)//' RCH"'

                VarSTR='variables="X","Y","Z","RCH"'
                nVar=3
            
                write(FNum,'(a)') trim(VarSTR)
            
                if(domain.Name == 'GWF') then
                    write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nCells/domain.nLayers,', t="'//trim(domain.name)//' RCH", datapacking=point'
                else if(domain.Name == 'SWF') then
                    write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nCells,', t="'//trim(domain.name)//' RCH", datapacking=point'
                else if(domain.Name == 'CLN') then
                    call ErrMsg('Code for ModflowDomainScatterToTecplot for CLN RCH domain required')
                end if    
        
                write(FNum,'(a)') trim(ZoneSTR)
                    !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                    !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
           
                do i=1,domain.nCells
                    if(domain.Name == 'GWF') then
                        if(Modflow.GWF.iLayer(i)==1) then
                            write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i),&
                                domain.Recharge(i)
                        endif
                    
                    else if(domain.Name == 'SWF') then
                        write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i),&
                            domain.Recharge(i)
                    endif
                end do

            
                call FreeUnit(FNum)
            end if

            if(domain.nSWBCCells > 0) then
                FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'_SWBC.tecplot.dat'
            
                call OpenAscii(FNum,FName)
                call Msg('  ')
                call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
                write(FNum,'(a)') 'Title = " Modflow '//trim(domain.name)//' SWBC"'

                VarSTR='variables="X","Y","Z","SWBC"'
                nVar=3
            
                write(FNum,'(a)') trim(VarSTR)
            
                write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nSWBCCells,', t="'//trim(domain.name)//' SWBC", datapacking=point'
        
                write(FNum,'(a)') trim(ZoneSTR)
                    !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                    !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
           
                do i=1,domain.nCells
                    if(bcheck(domain.Cell_is(i),CriticalDepth)) write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i),&
                        domain.CriticalDepthLength(i)
                end do
            
                call FreeUnit(FNum)
            end if
            if(domain.nDRNCells > 0) then
                FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'_DRN.tecplot.dat'
            
                call OpenAscii(FNum,FName)
                call Msg('  ')
                call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
                write(FNum,'(a)') 'Title = " Modflow '//trim(domain.name)//' DRN"'

                VarSTR='variables="X","Y","Z","DRN"'
                nVar=3
            
                write(FNum,'(a)') trim(VarSTR)
            
                write(ZoneSTR,'(a,i8,a)')'ZONE i=',domain.nDRNCells,', t="'//trim(domain.name)//' DRN", datapacking=point'
        
                write(FNum,'(a)') trim(ZoneSTR)
                    !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                    !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
           
                do i=1,domain.nCells
                    if(bcheck(domain.Cell_is(i),Drain)) write(FNum,'(4(1pg20.9))') domain.xCell(i),domain.yCell(i),domain.zCell(i),&
                        domain.DrainConductance(i)
                end do
            
                call FreeUnit(FNum)
            end if
        end if
    end subroutine ModflowDomainScatterToTecplot

    !-------------------------------------------------------------
    subroutine ModflowOutputToModflowStructure(FNumMUT, Modflow)
        implicit none
        !-------ASSIGN VERSION NUMBER AND DATE
        CHARACTER*40 VERSION
        CHARACTER*14 MFVNAM
        PARAMETER (VERSION='USG-TRANSPORT VERSION 2.02.1')
        PARAMETER (MFVNAM='USG-TRANSPORT ') !USG = Un-Structured Grids
        
        integer :: FNumMUT
        type (ModflowProject) Modflow
        
        integer :: i
       
        integer :: inunit
        CHARACTER*4 CUNIT(NIUNIT)
        DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'EVS ', 'GHB ',&  !  7  et time series is now EVS as ETS is for segmented ET&
                'RCH ', 'RTS ', 'TIB ', 'DPF ', 'OC  ', 'SMS ', 'PCB ',&  ! 14
                'BCT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',&  ! 21
                'LAK ', 'LPF ', 'DIS ', 'DISU', 'PVAL', 'SGB ', 'HOB ',&  ! 28
                'CLN ', 'DPT ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',&  ! 35
                'GNC ', 'DDF ', 'CHOB', 'ETS ', 'DRT ', 'QRT ', 'GMG ',&  ! 42
                'hyd ', 'SFR ', 'MDT ', 'GAGE', 'LVDA', 'SYF ', 'lmt6',&  ! 49
                'MNW1', '    ', '    ', 'KDEP', 'SUB ', 'UZF ', 'gwm ',&  ! 56
                'SWT ', 'PATH', 'PTH ', '    ', '    ', '    ', '    ',&  ! 63
                'TVM ', 'SWF ', 'SWBC', 34*'    '/

        integer :: maxunit, nc 

        INCLUDE 'openspec.inc'

        
        ! read prefix for project
        read(FNumMUT,'(a)') Modflow.Prefix
		call LwrCse(Modflow.Prefix)
        call Msg('Modflow project prefix: '//Modflow.Prefix)
        
        modflow.GWF.Name='GWF'
        
        
        ! Scan file
        Modflow.FNameSCAN=trim(Modflow.MUTPrefix)//'o.scan'
        open(Modflow.iSCAN,file=Modflow.FNameSCAN,status='unknown',form='formatted')
        write(Modflow.iSCAN,'(a)') 'Scan file from project '//trim(Modflow.Prefix)
        Modflow.nKeyWord=0
        allocate(Modflow.KeyWord(Modflow.nDim))
        Modflow.KeyWord(:)='UNDEFINED'


        ! Process NAM file
        Modflow.FNameNAM=trim(Modflow.Prefix)//'.nam'
        call openMUSGFile('NAM',' '//Modflow.FNameNAM,Modflow.Prefix,Modflow.iNAM,Modflow.FNameNAM)
        INUNIT = 99
        MAXUNIT= INUNIT
        !
        !4------OPEN NAME FILE.
        OPEN (UNIT=INUNIT,FILE=Modflow.FNameNAM,STATUS='OLD',ACTION=ACTION(1))
        NC=INDEX(Modflow.FNameNAM,' ')
        WRITE(*,490)' Using NAME file: ',Modflow.FNameNAM(1:NC)
490     FORMAT(A,A)
        
        ALLOCATE(IUNIT(NIUNIT))

        call Msg(' ')
        call Msg('-------Open and scan files listed in NAM file:')
        !
        !C2------Open all files in name file.
        CALL SGWF2BAS8OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,&
            VERSION,INBAS,MAXUNIT,modflow)
        
        !do i=1,niunit
        !    write(iout,*) i, iunit(i),cunit(i)
        !end do
        !
        
        ! Unit numbering starts at BCF6=7 so add 6 to iunut index
        Modflow.iBAS6 =inbas       
        file_open_flag(inbas) = .true.
        Modflow.iBCF6 =iunit(1)       
        Modflow.iWEL  =iunit(2)       
        Modflow.iDRN  =iunit(3)       
        Modflow.iRIV  =iunit(4)       
        Modflow.iEVT  =iunit(5)       
        Modflow.iEVS  =iunit(6)       
        Modflow.iGHB  =iunit(7)       
        Modflow.iRCH  =iunit(8)       
        Modflow.iRTS  =iunit(9)       
        Modflow.iTIB =iunit(10)       
        Modflow.iDPF =iunit(11)       
        Modflow.iOC  =iunit(12)       
        Modflow.iSMS =iunit(13)       
        Modflow.iPCB =iunit(14)       
        Modflow.iBCT =iunit(15)       
        Modflow.iFHB =iunit(16)       
        Modflow.iRES =iunit(17)       
        Modflow.iSTR =iunit(18)       
        Modflow.iIBS =iunit(19)       
        Modflow.iCHD =iunit(20)       
        Modflow.iHFB6=iunit(21)       
        Modflow.iLAK =iunit(22)       
        Modflow.iLPF =iunit(23)       
        Modflow.iDIS =iunit(24)       
        Modflow.iDISU=iunit(25)       
        Modflow.iPVAL=iunit(26)       
        Modflow.iSGB =iunit(27)       
        Modflow.iHOB =iunit(28)       
        Modflow.iCLN =iunit(29)       
        Modflow.iDPT =iunit(30)       
        Modflow.iZONE=iunit(31)       
        Modflow.iMULT=iunit(32)       
        Modflow.iDROB=iunit(33)       
        Modflow.iRVOB=iunit(34)       
        Modflow.iGBOB=iunit(35)       
        Modflow.iGNC =iunit(36)       
        Modflow.iDDF =iunit(37)       
        Modflow.iCHOB=iunit(38)       
        Modflow.iETS =iunit(39)       
        Modflow.iDRT =iunit(40)       
        Modflow.iQRT =iunit(41)       
        Modflow.iGMG =iunit(42)       
        Modflow.ihyd =iunit(43)       
        Modflow.iSFR =iunit(44)       
        Modflow.iMDT =iunit(45)       
        Modflow.iGAGE=iunit(46)       
        Modflow.iLVDA=iunit(47)       
        Modflow.iSYF =iunit(48)       
        Modflow.ilmt6=iunit(49)       
        Modflow.iMNW1=iunit(50)       
        Modflow.iKDEP=iunit(53)       
        Modflow.iSUB =iunit(54)       
        Modflow.iUZF =iunit(55)       
        Modflow.igwm =iunit(56)       
        Modflow.iSWT =iunit(57)       
        Modflow.iPATH=iunit(58)       
        Modflow.iPTH =iunit(59)       
        Modflow.iTVM =iunit(64)  
        Modflow.iSWF =iunit(65)   
        Modflow.iSWBC =iunit(66)   
        do i=1,66
            if(iunit(i) > 0) then
                file_open_flag(iunit(i)) = .true.
            end if
        end do

        ! First read all GSF (grid specification) files for GWF domain, then CLN and SWF domains if present
        call Msg(' ')
        call Msg('-------Read all GSF (grid specification) files:')
        Modflow.FNameGSF=trim(Modflow.Prefix)//'.GWF.gsf'
        inquire(file=Modflow.FNameGSF,exist=FileExists)
        if(.not. FileExists) then
            call Msg('No grid specification file: '//Modflow.FNameGSF)
        else
            call Msg('Modflow GWF GSF file: '//Modflow.FNameGSF)
	        call getunit(Modflow.iGSF)
            open(Modflow.iGSF,file=Modflow.FNameGSF,status='unknown',form='formatted')
        
            call Read_GWF_GSF(Modflow)
            
            modflow.GWF.ElementType='febrick'

        end if

        if(Modflow.iCLN /= 0) THEN
            Modflow.CLN.Name='CLN'
            modflow.CLN.ElementType='felineseg'
            Modflow.FNameCLN_GSF=trim(Modflow.Prefix)//'.CLN.gsf'
            inquire(file=Modflow.FNameCLN_GSF,exist=FileExists)
            if(.not. FileExists) then
                call Msg('No grid specification file: '//Modflow.FNameCLN_GSF)
            else
                call Msg('Modflow CLN GSF file: '//Modflow.FNameCLN_GSF)
	            call getunit(Modflow.iCLN_GSF)
                open(Modflow.iCLN_GSF,file=Modflow.FNameCLN_GSF,status='unknown',form='formatted')
        
                call Read_CLN_GSF(Modflow)
            end if
        end if

        if(Modflow.iSWF /= 0) THEN
            Modflow.SWF.name='SWF'
            Modflow.FNameSWF_GSF=trim(Modflow.Prefix)//'.SWF.gsf'
            inquire(file=Modflow.FNameSWF_GSF,exist=FileExists)
            if(.not. FileExists) then
                call Msg('No grid specification file: '//Modflow.FNameSWF_GSF)
            else
                call Msg('Modflow SWF GSF file: '//Modflow.FNameSWF_GSF)
	            call getunit(Modflow.iSWF_GSF)
                open(Modflow.iSWF_GSF,file=Modflow.FNameSWF_GSF,status='unknown',form='formatted')
        
                call Read_SWF_GSF(Modflow)
                
                if(Modflow.SWF.nNodesPerCell==3) then ! 3-node triangle, repeat node 3 for 4-node tecplot type fequadrilateral
                    modflow.SWF.ElementType='fetriangle'
                else if(Modflow.SWF.nNodesPerCell==4) then ! 4-node quadrilateral
                    modflow.SWF.ElementType='fequadrilateral'
                end if

            end if
        end if

        ! Read data in Modflow-USG order

        call Msg(' ')
        call Msg('-------Read options from BAS6:')
        call ReadBAS6_Options(Modflow) ! based on modflow routine SGWF2BAS8OPEN

        call Msg(' ')
        call Msg('-------Read first part of DISU:')
        call ReadDISU_pt1(Modflow)  ! based on modflow routine SDIS2GLO8AR
        NEQS = NODES

        IF(Modflow.iCLN/=0) THEN
            call Msg(' ')
            call Msg('-------Read data from CLN pt1:')
            call CLN_Read(Modflow)  ! based on modflow routine SDIS2CLN1AR
            NEQS = NEQS + NCLNNDS
            
            !C15A------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
            IF(NCONDUITYP.GT.0)THEN
                CALL  CLN_ReadCircularProperties(Modflow)! based on modflow routine SCLN2COND1RP
            ENDIF

            !C15B------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
            IF(NRECTYP.GT.0)THEN
                CALL  CLN_ReadRectangularProperties(Modflow) ! based on modflow routineSCLN2REC1RP
            ENDIF


            call ReadCLN_pt2(Modflow)  ! based on modflow routine SDIS2CLN1AR
        end if
        
        IF(Modflow.iSWF/=0) THEN
            call Msg(' ')
            call Msg('-------Read data from SWF pt1:')
            call ReadSWF(Modflow)  ! based on modflow routine SDIS2SWF1AR
            NEQS = NEQS + NSWFNDS

            ! Young-jin handles this in SDIS2SWF1AR above so I think not required
            !call ReadSWF_pt2(Modflow)  ! based on modflow routine SDIS2CLN1AR
        end if

        !crm not reading ghost node stuff yet
        !C---------------------------------------------------------------------
        !C3-----READ GNC PACKAGE INPUT  (CONNECTIVITIES AND FRACTIONS)
        !C---------------------------------------------------------------------

        
        !C5------ALLOCATE SPACE FOR PARAMETERS AND FLAGS.
        ALLOCATE(IA(NEQS+1))
        ALLOCATE (IBOUND(NEQS+1))
        ALLOCATE(AREA(NEQS))
        IA = 0


        call Msg(' ')
        call Msg('-------Read second part DISU:')
        WRITE(FNumEco,11) Modflow.iDISu
        11 FORMAT(1X,/1X,'DIS -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',&
            ' VERSION 1 : 5/17/2010 - INPUT READ FROM UNIT ',I4)
        if(Modflow.unstructured) then

            !C     *****************************************************************
            !C     READ AND SET NODLAY ARRAY, AND READ GEOMETRIC PARAMETERS AND
            !C     MATRIX CONNECTIVITY FOR UNSTRUCTURED GRID
            !C     *****************************************************************
            call ReadDISU_pt2(Modflow)  ! based on modflow routine SGWF2DIS8UR
            
            ! Hardwired to read CLN and FAHL arrays for now 
            call ReadDISU_pt3(Modflow)  

            !end if
        else
            ! call ReadDISU_StucturedGridData(Modflow)
        end if
        
        !!C--------------------------------------------------------------------------
        !!C7H------PREPARE IDXGLO ARRAY FOR CLN/SWF DOMAIN
        !IF(Modflow.iCLN/=0)THEN
        !    !CALL FILLIDXGLO_CLN
        !end if
        !IF(Modflow.iSWF/=0) THEN
        !    CALL FILLIDXGLO_SWF
        !end if


        call Msg(' ')
        call Msg('-------Read Stress Period Data from DISU:')
        call ReadDISU_StressPeriodData(Modflow)   
        
        
        !C7-----Allocate space for remaining global arrays.
        ALLOCATE (HNEW(NEQS))
        !ALLOCATE (HOLD(NEQS))
        !ALLOCATE (IFMBC)
        !IFMBC = 0
        !ALLOCATE (FMBE(NEQS))
        !ALLOCATE (Sn(NEQS),So(NEQS))
        !Sn = 1.0
        !So = 1.0
        !ALLOCATE (RHS(NEQS))
        !ALLOCATE (BUFF(NEQS))
        ALLOCATE (STRT(NEQS))
        !DDREF=>STRT
        !ALLOCATE (LAYHDT(NLAY))
        !ALLOCATE (LAYHDS(NLAY))
        !WRITE(IOUT,'(//)')

        !C------------------------------------------------------------------------
        !C10------Read rest of groundwater BAS Package file (IBOUND and initial heads)
        call Msg(' ')
        call Msg('-------Read IBOUND and initial heads from BAS6:')
        
        ALLOCATE (modflow.GWF.IBOUND(modflow.GWF.ncells))
        ALLOCATE (modflow.GWF.HNEW(Modflow.GWF.nCells))

        IF(IUNSTR.EQ.0)THEN
        !C10A-------FOR STRUCTURED GRIDS
            !CALL SGWF2BAS8SR
        ELSE
        !C10B-------FOR UNSTRUCTURED GRIDS
            CALL ReadBAS6_IBOUND_IHEADS(Modflow)  ! based on modflow routine SGWF2BAS8UR
        end if

        
        !C
        !C-----------------------------------------------------------------------
        !C11-----SET UP OUTPUT CONTROL.
        call Msg(' ')
        call Msg('-------Read data from OC:')
        CALL ReadOC(Modflow) ! based on modflow routine SGWF2BAS7I  
        
        IF(Modflow.iLPF/=0) THEN
            !C
            !C-----------------------------------------------------------------------
            !C11-----Read LPF Package file 
            call Msg(' ')
            call Msg('-------Read data from LPF:')
            CALL ReadLPF(Modflow) ! based on modflow routine SGWF2BAS7I  
        end if
        
        IF(Modflow.iCLN/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read rest of CLN Package file (IBOUND and initial heads)
            call Msg(' ')
            call Msg('-------Read IBOUND and initial heads from CLN:')
            ALLOCATE (modflow.CLN.IBOUND(modflow.CLN.ncells))
            ALLOCATE (modflow.CLN.HNEW(Modflow.CLN.nCells))
            CALL ReadCLN_IBOUND_IHEADS(Modflow)  ! based on modflow routine CLN2BAS1AR
        end if
        
        IF(Modflow.iSWF/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read rest of SWF Package file (IBOUND and initial heads)
            call Msg(' ')
            call Msg('-------Read IBOUND and initial heads from SWF:')
            ALLOCATE (modflow.SWF.IBOUND(modflow.SWF.ncells))
            ALLOCATE (modflow.SWF.HNEW(Modflow.SWF.nCells))
            CALL ReadSWF_IBOUND_IHEADS(Modflow)  ! based on modflow routine SWF2BAS1AR
        end if
        
        IF(Modflow.iWEL/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read WEL Package file
            call Msg(' ')
            call Msg('-------Read data from WEL:')
            CALL ReadWEL(Modflow)  ! based on modflow routine GWF2WEL7U1AR
        end if
        
        IF(Modflow.iCHD/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read CHD Package file
            call Msg(' ')
            call Msg('-------Read data from CHD:')
            CALL ReadCHD(Modflow)  ! based on modflow routine GWF2CHD7U1AR
        end if

        IF(Modflow.iRCH/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read RCH Package file
            call Msg(' ')
            call Msg('-------Read data from RCH:')
            CALL ReadRCH(Modflow)  ! based on modflow routine GWF2RCH8U1AR
            call ReadRCH_StressPeriods(Modflow) ! based on modflow routine GWF2RCH8U1RP
        end if
        
        IF(Modflow.iDRN/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read DRN Package file
            call Msg(' ')
            call Msg('-------Read data from DRN:')
            CALL ReadDRN(Modflow)  ! based on modflow routine GWF2RCH8U1AR
            call ReadDRN_StressPeriods(Modflow) ! based on modflow routine GWF2RCH8U1RP
        end if
        
        IF(Modflow.iSWBC/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read RCH Package file
            call Msg(' ')
            call Msg('-------Read data from SWBC:')
            CALL ReadSWBC(Modflow)  ! based on modflow routine SWF2BC1U1AR
            call ReadSWBC2(Modflow) ! based on modflow routine SWF2BC1U1RP
        end if
        
        IF(Modflow.iSMS/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read SMS Package file
            call Msg(' ')
            call Msg('-------Read data from SMS:')
            CALL ReadSMS(Modflow)  ! based on modflow routine SMS7U1AR
        end if

        call WriteVolumeBudgetToTecplot(Modflow)
        
        call CreateStepPeriodTimeFile(Modflow)

        
        call ReadBinary_HDS_File(Modflow,Modflow.GWF)
        call ReadBinary_DDN_File(Modflow,Modflow.GWF)
        call ReadBinary_CBB_File(Modflow, Modflow.GWF)
        if(Modflow.GWF.IsDefined) then
            call Msg(' ')
		    call Msg('Generating mesh-based Tecplot output files for GWF:')
            
            
            call ModflowResultsToTecplot(Modflow,Modflow.GWF)

            
        else
		   call Msg('Generating cell-based Tecplot output files for GWF:')
           call GWF_IBOUNDv2_ToTecplot(Modflow)
        end if
        
        IF(Modflow.iCLN/=0) THEN
            call ReadBinary_HDS_File(Modflow,Modflow.CLN)
            call ReadBinary_DDN_File(Modflow,Modflow.CLN)
            call ReadBinary_CBB_File(Modflow, Modflow.CLN)
            if(Modflow.CLN.IsDefined) then
    		    call Msg('Generating mesh-based Tecplot output files for CLN:')
    
                call ModflowResultsToTecplot(Modflow,Modflow.CLN)
                
            else
		       call Msg('No cell-based Tecplot output files for CLN:')
               !call CLN_IBOUNDv2_ToTecplot(Modflow)
            end if
                    
        end if
        
        IF(Modflow.iSWF/=0) THEN
            call ReadBinary_HDS_File(Modflow,Modflow.SWF)
            call ReadBinary_DDN_File(Modflow,Modflow.SWF)
            call ReadBinary_CBB_File(Modflow, Modflow.SWF)
            if(Modflow.SWF.IsDefined) then
    		    call Msg('Generating mesh-based Tecplot output files for SWF:')

                call ModflowResultsToTecplot(Modflow,Modflow.SWF)
                
            else
		       call Msg('No cell-based Tecplot output files for SWF:')
               !call CLN_IBOUNDv2_ToTecplot(Modflow)
            end if
                    
        end if
        




        !open(Modflow.iSCAN,file=Modflow.FNameSCAN,status='unknown',form='formatted')
        !write(Modflow.iSCAN,'(a)') 'Scan file from project '//trim(Modflow.Prefix)
        write(Modflow.iSCAN,'(a,i8,a)') 'Found ',Modflow.nKeyWord,' keywords'
        !do i=1,Modflow.nKeyWord
        !    write(Modflow.iSCAN,'(a)',iostat=status) Modflow.KeyWord(i)
        !end do
        close(Modflow.iSCAN)
        
    end subroutine ModflowOutputToModflowStructure
    
    !-------------------------------------------------------------
    subroutine ModflowResultsToTecplot(Modflow,domain)
        implicit none
        type (ModflowProject) Modflow
        type (ModflowDomain) Domain

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j, nvar, nVarShared

        character(4000) :: VarSharedStr


        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(domain.name)//'.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow Project: '//trim(Modflow.Prefix)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","'//trim(domain.name)//' z Cell","'//trim(domain.name)//' Layer","'//trim(domain.name)//' Ibound","'//trim(domain.name)//' Initial head",'
        nVar=7
       
        if(allocated(domain.head)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' Head",'
            nVar=nVar+1
        end if
        if(allocated(domain.Drawdown)) then
            if(domain.name == 'GWF') then
                VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' Saturation",'
            else if(domain.name == 'SWF') then
                VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' Depth",'
            else if(domain.name == 'CLN') then
                VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' Depth",'
            endif
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_STORAGE)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to STORAGE",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_CONSTANT_HEAD)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to CONSTANT_HEAD",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_RECHARGE)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to RECHARGE",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_WELLS)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to WELLS",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_DRAINS)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to DRAINS",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_CLN)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to CLN",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_SWF)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to SWF",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_GWF)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to GWF",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_FLOW_FACE)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to FLOW_FACES",'
            nVar=nVar+1
        end if
        if(allocated(domain.cbb_SWBC)) then
            VarSTR=trim(VarSTR)//'"'//trim(domain.name)//' to SWBC",'
            nVar=nVar+1
        end if
        
        
        write(FNum,'(a)') trim(VarSTR)
            
        write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="'//trim(domain.name)//'" SOLUTIONTIME=',modflow.TIMOT(1), &
            ',N=',domain.nNodes,', E=',domain.nElements,', datapacking=block, zonetype='//trim(domain.elementtype)
        
        if(Modflow.NodalControlVolume) then
            write(FNum,'(a)') trim(ZoneSTR)//&
                ', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                ', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
        
        else
        
            CellCenteredSTR=', VARLOCATION=([4'
            if(nVar.ge.5) then
                do j=5,nVar
                    write(str2,'(i2)') j
                    CellCenteredSTR=trim(CellCenteredSTR)//','//str2
                end do
            end if
            CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'

            write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)//&
                ', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                ', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
        endif
        

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (domain.x(i),i=1,domain.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (domain.y(i),i=1,domain.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (domain.z(i),i=1,domain.nNodes)
        write(FNum,'(a)') '# z cell'
        write(FNum,'(5e20.12)') (domain.zCell(i),i=1,domain.nCells)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (domain.iLayer(i),i=1,domain.nCells)
        write(FNum,'(a)') '# ibound'
        write(FNum,'(5i8)') (domain.ibound(i),i=1,domain.nCells)
        write(FNum,'(a)') '# hnew'
        write(FNum,'(5e20.12)') (domain.hnew(i),i=1,domain.nCells)
        nVarShared=7
       
        if(allocated(domain.head)) then
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (domain.head(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.Drawdown)) then
            write(FNum,'(a)') '# saturation'
            write(FNum,'(5e20.12)') (domain.Drawdown(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_STORAGE)) then
            write(FNum,'(a)') '# cbb_STORAGE'
            write(FNum,'(5e20.12)') (domain.cbb_STORAGE(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_CONSTANT_HEAD)) then
            write(FNum,'(a)') '# cbb_CONSTANT_HEAD'
            write(FNum,'(5e20.12)') (domain.cbb_CONSTANT_HEAD(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_RECHARGE)) then
            write(FNum,'(a)') '# cbb_RECHARGE'
            write(FNum,'(5e20.12)') (domain.cbb_RECHARGE(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_WELLS)) then
            write(FNum,'(a)') '# cbb_WELLS'
            write(FNum,'(5e20.12)') (domain.cbb_WELLS(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_DRAINS)) then
            write(FNum,'(a)') '# cbb_DRAINS'
            write(FNum,'(5e20.12)') (domain.cbb_DRAINS(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_CLN)) then
            write(FNum,'(a)') '# cbb_CLN'
            write(FNum,'(5e20.12)') (domain.cbb_CLN(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_SWF)) then
            write(FNum,'(a)') '# cbb_SWF'
            write(FNum,'(5e20.12)') (domain.cbb_SWF(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_GWF)) then
            write(FNum,'(a)') '# cbb_GWF'
            write(FNum,'(5e20.12)') (domain.cbb_GWF(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_FLOW_FACE)) then
            write(FNum,'(a)') '# cbb_FLOW_FACE'
            write(FNum,'(5e20.12)') (domain.cbb_FLOW_FACE(i,1),i=1,domain.nCells)
        end if
        if(allocated(domain.cbb_SWBC)) then
            write(FNum,'(a)') '# cbb_SWBC'
            write(FNum,'(5e20.12)') (domain.cbb_SWBC(i,1),i=1,domain.nCells)
        end if
        
        do i=1,domain.nElements
            if(domain.nNodesPerCell==8) then
                write(FNum,'(8i8)') (domain.iNode(j,i),j=1,domain.nNodesPerCell)
            else if(domain.nNodesPerCell==6) then
                write(FNum,'(8i8)') (domain.iNode(j,i),j=1,3), domain.iNode(3,i),(domain.iNode(j,i),j=4,6), domain.iNode(6,i)     
            else if(domain.nNodesPerCell==3) then
                write(FNum,'(8i8)') (domain.iNode(j,i),j=1,3)    
            else if(domain.nNodesPerCell==4) then
                write(FNum,'(8i8)') (domain.iNode(j,i),j=1,4)    
            else if(domain.nNodesPerCell==2) then
                write(FNum,'(8i8)') (domain.iNode(j,i),j=1,2)    
            end if
        end do
        
        VarSharedSTR=', VARSHARELIST=([1,2,3,4,5,6,7'
        if(nVarShared > 7) then
            do j=8,nVarShared
                write(str2,'(i2)') j
                VarSharedSTR=trim(VarSharedSTR)//','//str2
            end do
        end if
        VarSharedSTR=trim(VarSharedSTR)//'])'
        
        do j=2,Modflow.ntime
            
            write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="'//trim(domain.name)//'" SOLUTIONTIME=',modflow.TIMOT(j),',N=',domain.nNodes,', &
                E=',domain.nElements,', datapacking=block, zonetype='//trim(domain.elementtype)
            
            if(Modflow.NodalControlVolume) then
                write(FNum,'(a)') trim(ZoneSTR)// & 
                    trim(VarSharedSTR)//', CONNECTIVITYSHAREZONE=1 & 
                    , AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                    ', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
            else
        
                write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)&
                    //trim(VarSharedSTR)//', CONNECTIVITYSHAREZONE=1 & 
                    , AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
                    ', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
            end if
        
            if(allocated(domain.head)) then
                write(FNum,'(a)') '# head'
                write(FNum,'(5e20.12)') (domain.head(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.Drawdown)) then
                write(FNum,'(a)') '# saturation'
                write(FNum,'(5e20.12)') (domain.Drawdown(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_STORAGE)) then
                write(FNum,'(a)') '# cbb_STORAGE'
                write(FNum,'(5e20.12)') (domain.cbb_STORAGE(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_CONSTANT_HEAD)) then
                write(FNum,'(a)') '# cbb_CONSTANT_HEAD'
                write(FNum,'(5e20.12)') (domain.cbb_CONSTANT_HEAD(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_RECHARGE)) then
                write(FNum,'(a)') '# cbb_RECHARGE'
                write(FNum,'(5e20.12)') (domain.cbb_RECHARGE(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_WELLS)) then
                write(FNum,'(a)') '# cbb_WELLS'
                write(FNum,'(5e20.12)') (domain.cbb_WELLS(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_DRAINS)) then
                write(FNum,'(a)') '# cbb_DRAINS'
                write(FNum,'(5e20.12)') (domain.cbb_DRAINS(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_CLN)) then
                write(FNum,'(a)') '# cbb_CLN'
                write(FNum,'(5e20.12)') (domain.cbb_CLN(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_SWF)) then
                write(FNum,'(a)') '# cbb_SWF'
                write(FNum,'(5e20.12)') (domain.cbb_SWF(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_GWF)) then
                write(FNum,'(a)') '# cbb_GWF'
                write(FNum,'(5e20.12)') (domain.cbb_GWF(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_FLOW_FACE)) then
                write(FNum,'(a)') '# cbb_FLOW_FACE'
                write(FNum,'(5e20.12)') (domain.cbb_FLOW_FACE(i,j),i=1,domain.nCells)
            end if
            if(allocated(domain.cbb_SWBC)) then
                write(FNum,'(a)') '# cbb_SWBC'
                write(FNum,'(5e20.12)') (domain.cbb_SWBC(i,j),i=1,domain.nCells)
            end if
            !write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',modflow.TIMOT(j),',N=',domain.nNodes,', E=',domain.nCells,', datapacking=block, &
            !zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            !write(FNum,'(a)') '# head'
            !write(FNum,'(5e20.12)') (domain.head(i,j),i=1,domain.nCells)
            !write(FNum,'(a)') '# Drawdown'
            !write(FNum,'(5e20.12)') (domain.Drawdown(i,j),i=1,domain.nCells)
        end do
        
        call FreeUnit(FNum)

    end subroutine ModflowResultsToTecplot

    !-------------------------------------------------------------
    subroutine ModflowTMPLTScatterToTecplot(Modflow,TMPLT)
        implicit none
        type (ModflowProject) Modflow
        type(TecplotDomain) TMPLT
        
        integer :: i, j
        character(MAX_STR) :: FName
        
        if(TMPLT.InnerCircles) then
            FName=trim(Modflow.MUTPrefix)//'o.'//trim(TMPLT.name)//'_CircleCentres.tecplot.dat'
            
            call OpenAscii(FNum,FName)
            call Msg('  ')
            call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
            write(FNum,'(a)') 'Title = "'//trim(TMPLT.name)//' "'

            VarSTR='variables="X","Y"'
            nVar=3
            
            write(FNum,'(a)') trim(VarSTR)
            
            write(ZoneSTR,'(a,i8,a)')'ZONE i=',TMPLT.nElements,', t="'//trim(TMPLT.name)//' Circle Centres", datapacking=point'
        
            write(FNum,'(a)') trim(ZoneSTR)
            !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
            !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
           
            do i=1,TMPLT.nElements
                    write(FNum,'(3(1pg20.9))') TMPLT.xcircle(i),TMPLT.ycircle(i)
            end do

            call FreeUnit(FNum)
        endif
        
        if(Modflow.NodalControlVolume) then
            FName=trim(Modflow.MUTPrefix)//'o.'//trim(TMPLT.name)//'_EdgePoints.tecplot.dat'
            
            call OpenAscii(FNum,FName)
            call Msg('  ')
            call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
            write(FNum,'(a)') 'Title = "'//trim(TMPLT.name)//' "'

            VarSTR='variables="X","Y"'
            nVar=3
            
            write(FNum,'(a)') trim(VarSTR)
            
            write(ZoneSTR,'(a,i8,a)')'ZONE i=',TMPLT.nElements*TMPLT.nNodesPerElement,', t="'//trim(TMPLT.name)//' Edge Points", datapacking=point'
        
            write(FNum,'(a)') trim(ZoneSTR)
            !', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'//&
            !', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
           
            do i=1,TMPLT.nElements
                    do j=1,TMPLT.nNodesPerElement
                        write(FNum,'(3(1pg20.9))') TMPLT.xSide(j,i),TMPLT.ySide(j,i)
                    end do
            end do

            call FreeUnit(FNum)
        
        end if
    
    end subroutine ModflowTMPLTScatterToTecplot
    
     !----------------------------------------------------------------------
    subroutine NewZoneFromChosenCells(domain) 
        implicit none

        type (ModflowDomain) Domain

        integer :: i, j
	    integer :: ncount


        
        if(.not. allocated(domain.Zone_Is)) then 
            domain.nZones = 1
            allocate(domain.Zone_Is(domain.nZones),stat=ialloc)
            call AllocChk(ialloc,trim(domain.name)//' Zone_Is array')            
            domain.Zone_Is(:)=0
        else if(allocated(domain.Zone_Is)) then 
            call growIntegerArray(domain.Zone_Is,domain.nZones,domain.nZones+1)
            
            select case(ActiveDomain)
            case (iGWF)
                ! no zoned properties
            case (iSWF)
                call growRealArray(domain.Manning,domain.nZones,domain.nZones+1)
                call growRealArray(domain.DepressionStorageHeight,domain.nZones,domain.nZones+1)
                call growRealArray(domain.ObstructionStorageHeight,domain.nZones,domain.nZones+1)
                call growRealArray(domain.H1DepthForSmoothing,domain.nZones,domain.nZones+1)
                call growRealArray(domain.H2DepthForSmoothing,domain.nZones,domain.nZones+1)
            case (iCLN)
                call growIntegerArray(domain.Geometry,domain.nZones,domain.nZones+1)
                call growIntegerArray(domain.Direction,domain.nZones,domain.nZones+1)
                call growRealArray(domain.CircularRadius,domain.nZones,domain.nZones+1)
                call growRealArray(domain.RectangularWidth,domain.nZones,domain.nZones+1)
                call growRealArray(domain.RectangularHeight,domain.nZones,domain.nZones+1)
                call growRealArray(domain.LongitudinalK,domain.nZones,domain.nZones+1)
                call growIntegerArray(domain.FlowTreatment,domain.nZones,domain.nZones+1)
            end select
            
            domain.nZones = domain.nZones + 1
            domain.Zone_Is(:)=0 ! not chosen
        end if

        ncount=0
        do i=1,domain.nCells
            if(bcheck(domain.Cell_is(i),chosen)) then
                domain.iZone(i) = domain.nZones
                ncount=ncount+1
            end if
        end do

        write(TmpSTR,'(a,i10)') TAB//trim(domain.name)//' new zone number: ',domain.nZones
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,i10)') TAB//'Cells added to new zone: ',ncount
        call Msg(trim(TmpSTR))
	    if(ncount == 0) call ErrMsg('No Cells chosen')

    end subroutine NewZoneFromChosenCells
   !----------------------------------------------------------------------
    subroutine NodeCentredSWFCellGeometry(Modflow, TMPLT_SWF,TMPLT)
        implicit none
    
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_SWF
        integer :: i, j

        ! For modflow cell connection and area calculations
        integer :: j1, j2
        real(dr) :: TriangleArea

        ! Generate ia/ja, ConnectionLength and PerpendicularArea from element node lists 
        call NodeListToIaJaStructure(TMPLT_SWF,TMPLT)
            
        Modflow.SWF.njag=TMPLT_SWF.njag
        
        allocate(Modflow.SWF.ia(TMPLT_SWF.nNodes),Modflow.SWF.ja(TMPLT_SWF.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF ia, ja arrays')
        Modflow.SWF.ia(:)=TMPLT_SWF.ia
        Modflow.SWF.ja(:)=TMPLT_SWF.ja
        
        allocate(Modflow.SWF.ConnectionLength(TMPLT_SWF.njag),Modflow.SWF.PerpendicularArea(TMPLT_SWF.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF Cell connection length, perpendicular area array')
        Modflow.SWF.ConnectionLength(:)= TMPLT_SWF.ConnectionLength(:)
        Modflow.SWF.PerpendicularArea(:)=TMPLT_SWF.PerpendicularArea(:)
      
        ! Cell horizontal areas
        ! calculate node-centred cell area based on TMPLT.CellArea
        allocate(Modflow.SWF.CellArea(TMPLT.nNodes),stat=ialloc)
        call AllocChk(ialloc,'SWF cell horizontal area arrays')
        Modflow.SWF.CellArea(:)=0.0d0
        do i=1,TMPLT.nElements
            do j=1,TMPLT.nNodesPerElement
                j1=TMPLT.iNode(j,i)
                if(j < TMPLT.nNodesPerElement) then
                    j2=TMPLT.iNode(j+1,i)
                else
                    j2=TMPLT.iNode(1,i)
                end if
                
                call area_triangle(TMPLT.x(j1),TMPLT.y(j1),0.0d0, &
                        &          TMPLT.xSide(j,i),TMPLT.ySide(j,i),0.0d0, &
                        &          TMPLT.xCircle(i),TMPLT.yCircle(i),0.0d0, &
                        &          TriangleArea)
                Modflow.SWF.CellArea(j1)=Modflow.SWF.CellArea(j1)+TriangleArea
        
                call area_triangle(TMPLT.x(j2),TMPLT.y(j2),0.0d0, &
                        &          TMPLT.xCircle(i),TMPLT.yCircle(i),0.0d0, &
                        &          TMPLT.xSide(j,i),TMPLT.ySide(j,i),0.0d0, &
                        &          TriangleArea)
                Modflow.SWF.CellArea(j2)=Modflow.SWF.CellArea(j2)+TriangleArea
            end do
        end do
   
    end subroutine NodeCentredSWFCellGeometry

    !-------------------------------------------------------------
    subroutine NodeCentredSWFIaJaStructureToGWF(TMPLT_SWF,TMPLT_GWF)
        implicit none
        type(TecplotDomain) TMPLT_SWF
        type(TecplotDomain) TMPLT_GWF
        
        integer, parameter :: MAXCONNECTIONS=20

        integer :: i, j, k, iGWF_Cell, kCell
        
        
        integer :: nCellsGWF
        
        integer , allocatable :: ia_TMP(:)
        integer , allocatable :: ja_TMP(:,:)
        integer , allocatable :: ja_TMP2(:,:)
        real(dr) , allocatable :: ConnectionLength_TMP(:,:)
        real(dr) , allocatable :: PerpendicularArea_TMP(:,:)
        
        
        integer :: iSort(MAXCONNECTIONS)

        integer :: iNjag
        
        nCellsGWF=TMPLT_SWF.nNodes*(TMPLT_GWF.nLayers+1)
        
        allocate(ia_TMP(nCellsGWF), ja_TMP(MAXCONNECTIONS,nCellsGWF), ja_TMP2(MAXCONNECTIONS,nCellsGWF), &
            ConnectionLength_TMP(MAXCONNECTIONS,nCellsGWF),PerpendicularArea_TMP(MAXCONNECTIONS,nCellsGWF), stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' cell build arrays')
        

        
        call Msg(' ')
        call Msg('  Generating IA/JA and cell connection arrays for domain '//trim(TMPLT_GWF.name)//'...')
        
        ia_TMP(:)=0
        ja_TMP(:,:)=0
        ConnectionLength_TMP(:,:)=0
        PerpendicularArea_TMP(:,:)=0
        
        ! Form GWF ia, ja from SWF ia, ja
        do j=1,TMPLT_GWF.nLayers+1
            iNjag=0
            do i=1,TMPLT_SWF.nNodes
                iGWF_Cell=(j-1)*TMPLT_SWF.nNodes+i
                ia_TMP(iGWF_Cell)=TMPLT_SWF.ia(i)
                do k=1,TMPLT_SWF.ia(i)
                    iNjag=iNjag+1
                    kCell=(j-1)*TMPLT_SWF.nNodes+TMPLT_SWF.ja(iNJag)
                    if(k==1) kCell=-kCell  ! so first entry is always sorted to beginning of list
                    ja_TMP(k,iGWF_Cell)=kCell
                    
                    ! For now, assume we can inherit these from SWF domain.  Should really come from TMPLT.
                    ConnectionLength_TMP(k,iGWF_Cell)=TMPLT_SWF.ConnectionLength(iNjag)
                    PerpendicularArea_TMP(k,iGWF_Cell)=TMPLT_SWF.PerpendicularArea(iNjag)
                end do
                    
                if(j < TMPLT_GWF.nLayers+1) then ! downward connection
                    ia_TMP(iGWF_Cell)=ia_TMP(iGWF_Cell)+1
                    ja_TMP(ia_TMP(iGWF_Cell),iGWF_Cell)=iGWF_Cell+TMPLT_SWF.nNodes
                end if
                if(j > 1) then ! upward connection
                    ia_TMP(iGWF_Cell)=ia_TMP(iGWF_Cell)+1
                    ja_TMP(ia_TMP(iGWF_Cell),iGWF_Cell)=iGWF_Cell-TMPLT_SWF.nNodes
                end if
            end do
        end do
                        
                        
        ! Sort cell connection list, remove duplicates and determine ia
        allocate(TMPLT_GWF.ia(nCellsGWF),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' ia array')
        !write(TMPStr,'(a)') ' Cell   ConnectionLength   PerendicularArea'
        !call Msg(trim(TMPStr))
        do i=1,nCellsGWF
            call indexx_int(MAXCONNECTIONS,ja_TMP(:,i),iSort)
            !do j=1,MAXCONNECTIONS 
            !    if(ja_TMP(isort(j),i) /= 0) then
            !        write(TMPStr,'(i4,2(1pg20.5))') ja_TMP(isort(j),i) ,ConnectionLength_TMP(isort(j),i),PerpendicularArea_TMP(isort(j),i)
            !        call Msg(trim(TMPStr))
            !    end if
            !end do
            TMPLT_GWF.ia(i)=0
            !ja_TMP2(1,i)=i
            do j=1,MAXCONNECTIONS
                if(ja_TMP(isort(j),i) == 0) cycle
                !if(ja_TMP(isort(j),i) == abs(ja_TMP(isort(j-1),i)) ) cycle  
                TMPLT_GWF.ia(i)=TMPLT_GWF.ia(i)+1
                ja_TMP2(TMPLT_GWF.ia(i),i)=ja_TMP(isort(j),i)
            end do
!write(*,'(a,20i4)') 'final   ', (ja_TMP2(j,i),j=1,TMPLT_GWF.ia(i))
        end do
        
        ! Determine size of ja (njag) and copy ja_TMP to ja 
        TMPLT_GWF.njag=0
        do i=1,nCellsGWF
            TMPLT_GWF.njag=TMPLT_GWF.njag+TMPLT_GWF.ia(i)
        end do
        allocate(TMPLT_GWF.ja(TMPLT_GWF.njag),TMPLT_GWF.jaElement(TMPLT_GWF.njag),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' node neighbour ja, jaElement array')
        allocate(TMPLT_GWF.ConnectionLength(TMPLT_GWF.njag),TMPLT_GWF.PerpendicularArea(TMPLT_GWF.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF Cell connection length, perpendicular area array')
        TMPLT_GWF.ConnectionLength(:)=0.0d0
        TMPLT_GWF.PerpendicularArea(:)=0.0d0

        
        iNJag=0
        do i=1,nCellsGWF
            do j=1,TMPLT_GWF.ia(i)
                iNjag=iNjag+1
                TMPLT_GWF.ja(iNJag)=abs(ja_TMP2(j,i))  ! restore first entry to positive number
                !TMPLT_GWF.jaElement(iNJag)=ja_TMP2_Element(j,i)
                TMPLT_GWF.ConnectionLength(iNjag)=ConnectionLength_TMP(j,i)
                TMPLT_GWF.PerpendicularArea(iNjag)=PerpendicularArea_TMP(j,i)
            end do
        end do
           
        return
    end subroutine NodeCentredSWFIaJaStructureToGWF

    !-------------------------------------------------------------
    subroutine NodeListToIaJaStructure(domain,TMPLT)
        implicit none
        type(TecplotDomain) domain
        type(TecplotDomain) TMPLT
        
        integer, parameter :: MAXCONNECTIONS=20

        integer :: i, j, jNode, kNode
        
        real(dr) :: FractionSide
        
        integer :: ia_TMP(domain.nNodes)
        integer :: ja_TMP(MAXCONNECTIONS,domain.nNodes)
        integer :: ja_TMP2(MAXCONNECTIONS,domain.nNodes)
        integer :: ja_TMP2_element(MAXCONNECTIONS,domain.nNodes)
        real(dr) :: ConnectionLength_TMP(MAXCONNECTIONS,domain.nNodes)
        real(dr) :: PerpendicularArea_TMP(MAXCONNECTIONS,domain.nNodes)
        
        
        integer :: iSort(MAXCONNECTIONS)

        integer :: iNjag, iConn
        
        ! For xSide, ySide intercept calc
	    real(dr) :: xc1, yc1, xc2, yc2
	    real(dr) :: xs1, ys1, xs2, ys2
	    !real(dr) :: x_tmp, y_tmp
	    real(dr) :: del, del2
	    real(dr) :: rseg, rcut
        
        ! For xSide, ySide circle tangent
        integer :: j1, j2
        real(dr) :: D, DC, D1, D2, RC


        ! For modflow cell connection and area calculations we need xSide, ySide array coordinates
        allocate(TMPLT.xSide(TMPLT.nNodesPerElement,TMPLT.nElements),&
                    TMPLT.ySide(TMPLT.nNodesPerElement,TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'GB xSide, ySide')
        
        ! xSide, ySide at circle centre intersections for neighbouring elements
        iNjag=0
        iConn=0
        do i=1,TMPLT.nElements
            iNjag=iNjag+1
            do j=2,TMPLT.ia(i)
                iConn=iConn+1
                iNjag=iNjag+1
                !write(TmpSTR,*) i, TMPLT.element(iconn), TMPLT.face(iconn), TMPLT.neighbour(iconn)
                !call Msg(Trim(TmpSTR))
                
                ! Coordinates of face endpoints
                xc1=TMPLT.x(TMPLT.iNode(TMPLT.face(iConn),TMPLT.element(iconn)))
			    yc1=TMPLT.y(TMPLT.iNode(TMPLT.face(iConn),TMPLT.element(iconn)))
                if(TMPLT.face(iConn) < TMPLT.nNodesPerElement) then ! connect to next node
                    xc2=TMPLT.x(TMPLT.iNode(TMPLT.face(iConn)+1,TMPLT.element(iconn)))
			        yc2=TMPLT.y(TMPLT.iNode(TMPLT.face(iConn)+1,TMPLT.element(iconn)))
                else ! connect to node 1
                    xc2=TMPLT.x(TMPLT.iNode(1,TMPLT.element(iconn)))
			        yc2=TMPLT.y(TMPLT.iNode(1,TMPLT.element(iconn)))
                end if
                
                ! Coordinates of neighbour circle centres
			    xs1=TMPLT.xCircle(TMPLT.element(iconn))
			    ys1=TMPLT.yCircle(TMPLT.element(iconn))
			    xs2=TMPLT.xCircle(TMPLT.neighbour(iconn))
			    ys2=TMPLT.yCircle(TMPLT.neighbour(iconn))
			    !
			    !  The following 6 lines determine if the two segments intersect
			    del=(xs1-xs2)*(yc2-yc1)-(ys1-ys2)*(xc2-xc1)
			    del2=(xc1-xc2)*(ys2-ys1)-(yc1-yc2)*(xs2-xs1)
			    if (abs(del).gt.0.0 .and. abs(del2).gt.0.0) then
				    rseg=1.-((yc2-yc1)*(xc2-xs2)-(xc2-xc1)*(yc2-ys2))/del
				    rcut=1.-((ys2-ys1)*(xs2-xc2)-(xs2-xs1)*(ys2-yc2))/del2
				    if (rseg.ge.0.0  .AND. rseg.le.1.0 .AND. rcut.ge.0.0 .AND. rcut.le.1.0)    then
					    TMPLT.xSide(TMPLT.face(iConn),TMPLT.element(iconn))=xs1*(1.0-rseg)+xs2*rseg
					    TMPLT.ySide(TMPLT.face(iConn),TMPLT.element(iconn))=ys1*(1.0-rseg)+ys2*rseg
                    else
                        call ErrMsg('Lines do not intersect')
                    end if
                end if

            end do
        end do
        
        ! xSide, ySide at circle tangent for boundary element sides
        do i=1,TMPLT.nElements
            do j=1,TMPLT.nNodesPerElement
                j1=TMPLT.iNode(j,i)
                if(j < TMPLT.nNodesPerElement) then
                    j2=TMPLT.iNode(j+1,i)
                else
                    j2=TMPLT.iNode(1,i)
                end if
                if(bcheck(TMPLT.Node_is(j1),BoundaryNode) .and. bcheck(TMPLT.Node_is(j2),BoundaryNode)) then ! boundary segment
                
                    RC=TMPLT.rCircle(i)
                    DC=sqrt((TMPLT.xCircle(i)-TMPLT.x(j1))**2+(TMPLT.yCircle(i)-TMPLT.y(j1))**2)
                    D=TMPLT.SideLength(j,i)
                    D1=sqrt(DC*DC-RC*RC)
                    D2=D-D1
                
                    TMPLT.xSide(j,i)=TMPLT.x(j1)+(TMPLT.x(j2)-TMPLT.x(j1))*D1/D
                    TMPLT.ySide(j,i)=TMPLT.y(j1)+(TMPLT.y(j2)-TMPLT.y(j1))*D1/D
                end if
                
            end do
        end do
                
                

        
        call Msg(' ')
        call Msg('  Generating IA/JA and cell connection arrays for domain '//trim(domain.name)//'...')
        
        ja_TMP(:,:)=0
        ja_TMP2(:,:)=0
        ConnectionLength_TMP(:,:)=0
        PerpendicularArea_TMP(:,:)=0
        
        do i=1,domain.nNodes
            ia_TMP(i)=1
            ja_TMP(ia_TMP(i),i)=-i
        end do

        ! Loop in order around nodes in element and form cell connection lists
        do i=1,domain.nElements
            do j=1,domain.nNodesPerElement
                jNode=domain.iNode(j,i)
                if(j == domain.nNodesPerElement) then
                    kNode=domain.iNode(1,i)
                else
                    kNode=domain.iNode(J+1,i)
                end if
                
                ! jnode 
                ia_TMP(jNode)=ia_TMP(jNode)+1
                ja_TMP(ia_TMP(jNode),jNode)=kNode
                ! Fraction (0 to 1) of distance from jNode to xSide, ySide
                FractionSide=sqrt((TMPLT.x(jNode)-TMPLT.xSide(j,i))**2+(TMPLT.y(jNode)-TMPLT.ySide(j,i))**2)/TMPLT.SideLength(j,i)
                !ConnectionLength_TMP(ia_TMP(jNode),jNode)=ConnectionLength_TMP(ia_TMP(jNode),jNode)+FractionSide*TMPLT.SideLength(j,i)
                ConnectionLength_TMP(ia_TMP(jNode),jNode)=FractionSide*TMPLT.SideLength(j,i)
                PerpendicularArea_TMP(ia_TMP(jNode),jNode)=TMPLT.rCircle(i)

                ! knode 
                ia_TMP(kNode)=ia_TMP(kNode)+1
                ja_TMP(ia_TMP(kNode),kNode)=jNode
                ! Fraction (0 to 1) of distance from jNode to xSide, ySide
                !ConnectionLength_TMP(ia_TMP(kNode),kNode)=ConnectionLength_TMP(ia_TMP(kNode),kNode)+(1.0d0-FractionSide)*TMPLT.SideLength(j,i)
                ConnectionLength_TMP(ia_TMP(kNode),kNode)=(1.0d0-FractionSide)*TMPLT.SideLength(j,i)
                PerpendicularArea_TMP(ia_TMP(kNode),kNode)=TMPLT.rCircle(i)
                
            end do
        end do
        
        ! Sort cell connection list, remove duplicates and determine ia
        allocate(domain.ia(domain.nNodes),stat=ialloc)
        call AllocChk(ialloc,trim(domain.name)//' node neighbour ia, ja_TMP arrays')
        !write(TMPStr,'(a)') ' Cell   ConnectionLength   PerendicularArea'
        !call Msg(trim(TMPStr))
        do i=1,domain.nNodes
            call indexx_int(MAXCONNECTIONS,ja_TMP(:,i),iSort)
            !do j=1,MAXCONNECTIONS 
            !    if(ja_TMP(isort(j),i) /= 0) then
            !        write(TMPStr,'(i4,2(1pg20.5))') ja_TMP(isort(j),i),ConnectionLength_TMP(isort(j),i),PerpendicularArea_TMP(isort(j),i)
            !        call Msg(trim(TMPStr))
            !    end if
            !end do
            domain.ia(i)=1
            ja_TMP2(1,i)=i
            do j=2,MAXCONNECTIONS
                if(ja_TMP(isort(j),i) == 0) cycle
                if(ja_TMP(isort(j),i) == abs(ja_TMP(isort(j-1),i)) ) cycle  
                domain.ia(i)=domain.ia(i)+1
                ja_TMP2(domain.ia(i),i)=ja_TMP(isort(j),i)
            end do
!write(*,'(a,20i4)') 'final   ', (ja_TMP2(j,i),j=1,domain.ia(i))
        end do
        
        ! Determine size of ja (njag) and copy ja_TMP to ja 
        domain.njag=0
        do i=1,domain.nNodes
            domain.njag=domain.njag+domain.ia(i)
        end do
        allocate(domain.ja(domain.njag),domain.jaElement(domain.njag),stat=ialloc)
        call AllocChk(ialloc,trim(domain.name)//' node neighbour ja, jaElement array')
        allocate(domain.ConnectionLength(domain.njag),domain.PerpendicularArea(domain.njag),stat=ialloc)
        call AllocChk(ialloc,'SWF Cell connection length, perpendicular area array')
        domain.ConnectionLength(:)=0.0d0
        domain.PerpendicularArea(:)=0.0d0

        
        iNJag=0
        do i=1,domain.nNodes
            do j=1,domain.ia(i)
                iNjag=iNjag+1
                domain.ja(iNJag)=ja_TMP2(j,i)
                domain.jaElement(iNJag)=ja_TMP2_Element(j,i)
                domain.ConnectionLength(iNjag)=ConnectionLength_TMP(j,i)
                domain.PerpendicularArea(iNjag)=PerpendicularArea_TMP(j,i)
            end do
        end do
           
        return
    end subroutine NodeListToIaJaStructure
    
    !-------------------------------------------------------------
    subroutine openBinaryMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
        l1=index(line,trim(Prefix))-1

        ! check for path string before prefix
        if(line(l1:l1) .eq. '/' .or. line(l1:l1) .eq. '\') then
            l1=l1-1
            do
                if(line(l1:l1) .eq. BLANK .or. line(l1:l1) .eq. TAB ) exit
                !write(*,*) ichar(line(l1:l1)), line(l1:l1)
                l1=l1-1
            end do   
        end if    
        FName=line(l1+1:)
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('No file found: '//FName)
        end if
        call Msg('Opened binary '//trim(FileType)//' file: '//FName)
	    call getunit(iUnit)
        open(iUnit,file=FName,status='old',form='binary',action='read')  
        write(TmpSTR,'(i5)') iUnit
        call Msg('Reading from unit: '//trim(TmpSTR))
        
    end subroutine openBinaryMUSGFile

    !-------------------------------------------------------------
    subroutine openMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
        l1=index(line,trim(Prefix))-1

        ! check for path string before prefix
        if(line(l1:l1) .eq. '/' .or. line(l1:l1) .eq. '\') then
            l1=l1-1
            do
                if(line(l1:l1) .eq. BLANK) exit
                l1=l1-1
            end do   
        end if    
        FName=line(l1:)
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('No file found: '//FName)
        end if
	    call getunit(iUnit)
        open(iUnit,file=FName,status='unknown',form='formatted')  
        call Msg('Opened ascii '//trim(FileType)//' file: '//trim(FName))
        write(TmpSTR,'(i5)') iUnit
        call Msg('Reading from unit: '//trim(TmpSTR))
        
    end subroutine openMUSGFile
    
    !-------------------------------------------------------------
    subroutine PostprocessExistingModflowModel(FNumMUT, Modflow,prefix) !--- Post-process existing Modflow model from instructions
        implicit none

        integer :: FNumMUT
        character(*) :: prefix
        type (ModflowProject) Modflow
        
        Modflow.MUTPrefix=prefix
           
        call ModflowOutputToModflowStructure(FNumMUT, Modflow)
    
    end subroutine PostprocessExistingModflowModel
   
    !-------------------------------------------------------------
    subroutine ScanFile(FNum,Modflow)
        implicit none

        type (ModflowProject) Modflow
        
        integer :: Fnum
        integer :: i
     
        character(MAX_STR) :: line
        character(MAX_STR) :: PossibleKey
        
    
        do 
            read(FNum,'(a)',iostat=status) line
            call LwrCse(line)
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                write(Modflow.iSCAN,'(a)',iostat=status) trim(line)
                cycle
            end if
            
            do i=1,len_trim(line)
                !write(*,*) i,ichar(line(i:i))
                if(ichar(line(i:i)) .ge. 42 .and. ichar(line(i:i)) .le. 57 .or. ichar(line(i:i)) .eq. 32) cycle
                if(line(i:i) .eq. 'e' .or.  &
                    line(i:i) .eq. 'd' .or.   &
                    line(i:i) .eq. 'g') then
                    if(line(i+1:i+1) .eq. '+' .or. line(i+1:i+1) .eq. '-') cycle
                end if
                if(i.eq.1) then
                    PossibleKey=line(i:)
                else
                    PossibleKey=line(i-1:)
                end if
                call AddToScan(PossibleKey, Modflow)
                exit
            end do
        end do
        
        rewind(FNum)    
            
    end subroutine ScanFile

    !----------------------------------------------------------------------
    subroutine SMSParamterSetNumber(FNumMUT) 
        implicit none

        integer :: FNumMUT
        
        real :: LengthConversionFactor

        read(FNumMUT,*) iSMSParameterSet
        write(TmpSTR,'(i4)') iSMSParameterSet
        call Msg(TAB//'Using SMS parameter set '//trim(TmpSTR)//', '//trim(SMS_Name(iSMSParameterSet)))
        
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'OUTER ITERATION CONVERGENCE CRITERION  (HCLOSE)        ',SMS_HCLOSE(iSMSParameterSet),'     '//TRIM(SMS_LengthUnit(iSMSParameterSet))
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5,a)')TAB//'INNER ITERATION CONVERGENCE CRITERION  (HICLOSE)       ',SMS_HICLOSE(iSMSParameterSet),'     '//TRIM(SMS_LengthUnit(iSMSParameterSet))
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'MAXIMUM NUMBER OF OUTER ITERATIONS     (MXITER)        ',SMS_MXITER(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'MAXIMUM NUMBER OF INNER ITERATIONS     (ITER1)         ',SMS_ITER1(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'SOLVER PRINTOUT INDEX                  (IPRSMS)        ',SMS_IPRSMS(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'NONLINEAR ITERATION METHOD             (NONLINMETH)    ',SMS_NONLINMETH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'LINEAR SOLUTION METHOD                 (LINMETH)       ',SMS_NONLINMETH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'D-B-D WEIGHT REDUCTION FACTOR          (THETA)         ',SMS_THETA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'D-B-D WEIGHT INCREASE INCREMENT        (KAPPA)         ',SMS_KAPPA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'D-B-D PREVIOUS HISTORY FACTOR          (GAMMA)         ',SMS_GAMMA(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'MOMENTUM TERM                          (AMOMENTUM)     ',SMS_AMOMENTUM(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'MAXIMUM NUMBER OF BACKTRACKS           (NUMTRACK)      ',SMS_NUMTRACK(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'BACKTRACKING TOLERANCE FACTOR          (BTOL)          ',SMS_BTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'BACKTRACKING REDUCTION FACTOR          (BREDUC)        ',SMS_BREDUC(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'BACKTRACKING RESIDUAL LIMIT            (RES_LIM)       ',SMS_RES_LIM(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'TRUNCATED NEWTON FLAG                  (ITRUNCNEWTON)  ',SMS_ITRUNCNEWTON(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,a)')      TAB//'Options                                                ',SMS_Options(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'ACCELERATION METHOD                    (IACL)          ',SMS_IACL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'EQUATION ORDERING FLAG                 (NORDER)        ',SMS_NORDER(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'LEVEL OF FILL                          (LEVEL)         ',SMS_LEVEL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'MAXIMUM NUMBER OF ORTHOGONALIZATIONS   (NORTH)         ',SMS_NORTH(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'INDEX FOR USING REDUCED SYSTEM         (IREDSYS)       ',SMS_IREDSYS(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'RESIDUAL REDUCTION CONVERGE CRITERION  (RRCTOL)        ',SMS_RRCTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'INDEX FOR USING DROP TOLERANCE         (IDROPTOL)      ',SMS_IDROPTOL(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,1pg15.5)')TAB//'DROP TOLERANCE VALUE                   (EPSRN)         ',SMS_EPSRN(iSMSParameterSet)
        call Msg(trim(TmpSTR))
        
        LengthConversionFactor=LengthConverter(UnitsOfLength,SMS_LengthUnit(iSMSParameterSet))
        if(LengthConversionFactor /= 1.0) then
            call Msg(' ')
            call Msg(TAB//'**** Length Units Conversion **** ')
            write(TmpSTR,'(a)')TAB//'SMS parameter set length unit:    '//trim(SMS_LengthUnit(iSMSParameterSet))
            call Msg(TmpSTR)
            write(TmpSTR,'(a)')TAB//'Modflow length unit:     '//trim(UnitsOfLength)
            call Msg(TmpSTR)
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'Length Conversion Factor:     ',LengthConversionFactor,'     '//TRIM(UnitsOfLength)//' per '//TRIM(SMS_LengthUnit(iSMSParameterSet))
            call Msg(TmpSTR)
        endif
        
        if(LengthConversionFactor /= 1.0) then
            SMS_HCLOSE(iSMSParameterSet)=SMS_HCLOSE(iSMSParameterSet)*LengthConversionFactor
            SMS_HICLOSE(iSMSParameterSet)=SMS_HICLOSE(iSMSParameterSet)*LengthConversionFactor

            call Msg(' ')
            call Msg(TAB//'**** After Unit Conversion **** ')
            write(TmpSTR,'(i5)') iSMSParameterSet
		    call Msg(TAB//'Properties of SMS dataset '//trim(TmpSTR)//', '//trim(SMS_Name(iSMSParameterSet))//' after unit conversion')
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'OUTER ITERATION CONVERGENCE CRITERION  (HCLOSE)        ',SMS_HCLOSE(iSMSParameterSet),'     '//TRIM(UnitsOfLength)
            call Msg(trim(TmpSTR))
            write(TmpSTR,'(a,1pg15.5,a)')TAB//'INNER ITERATION CONVERGENCE CRITERION  (HICLOSE)       ',SMS_HICLOSE(iSMSParameterSet),'     '//TRIM(UnitsOfLength)
            call Msg(trim(TmpSTR))
       end if     


    end subroutine SMSParamterSetNumber
   
    !----------------------------------------------------------------------
    subroutine StressPeriod(FNumMUT,modflow)
        implicit none
        
        integer :: FNumMUT
        type (ModflowProject) Modflow
        
        integer, parameter :: MAXStressPeriods=100
        character(MAX_INST) :: Type_CMD	                =   'type'
        character(MAX_INST) :: Duration_CMD	            =   'duration'
        character(MAX_INST) :: NumberOfTimesteps_CMD	=   'number of timesteps'
        character(MAX_INST) :: Deltat_CMD	            =   'deltat'
        character(MAX_INST) :: Tminat_CMD	            =   'tminat'
        character(MAX_INST) :: Tmaxat_CMD	            =   'tmaxat'
        character(MAX_INST) :: Tadjat_CMD	            =   'tadjat'
        character(MAX_INST) :: Tcutat_CMD	            =   'tcutat'
        Modflow.nPeriods=Modflow.nPeriods+1  
        write(TmpSTR,'(a,i8)')TAB//'Stress period ',Modflow.nPeriods
        call Msg(trim(TmpSTR))
        
        if(Modflow.nPeriods == 1) then
            allocate(Modflow.StressPeriodDuration(MAXStressPeriods), Modflow.StressPeriodnTsteps(MAXStressPeriods), &
            Modflow.StressPeriodnTstepMult(MAXStressPeriods), Modflow.StressPeriodType(MAXStressPeriods),stat=ialloc)
            Modflow.StressPeriodDuration(:)=1.0d0
            Modflow.StressPeriodnTsteps(:)=1
            Modflow.StressPeriodnTstepMult(:)=1.1d0
            Modflow.StressPeriodType(:)='TR'
        end if
        
        read_StressPeriod_instructions: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            
            if(index(instruction,'end') /=0) then
                call Msg('end stress period')
                exit read_StressPeriod_instructions
            end if

            if(index(instruction,Type_cmd) /=0) then
                read(FNumMUT,'(a)') modflow.StressPeriodType(Modflow.nPeriods)
                if(modflow.StressPeriodType(Modflow.nPeriods) /= 'SS' .and. modflow.StressPeriodType(Modflow.nPeriods) /= 'TR') then
                    call ErrMsg('Stress Period type must begin with either SS or TR')
                end if
                write(TmpSTR,'(a,1pg12.4)')TAB//TAB//'Type: ',modflow.StressPeriodType(Modflow.nPeriods)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Duration_cmd) /=0) then
                read(FNumMUT,*) modflow.StressPeriodDuration(Modflow.nPeriods)
                write(TmpSTR,'(a,1pg12.4,a)')TAB//TAB//'Duration: ',modflow.StressPeriodDuration(Modflow.nPeriods),'     '//TRIM(modflow.STR_TimeUnit)
                call Msg(trim(TmpSTR))
                
            else if(index(instruction,NumberOfTimesteps_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodnTsteps(Modflow.nPeriods)
                write(TmpSTR,'(a,i5)')TAB//TAB//'Number of time steps: ',modflow.StressPeriodnTsteps(Modflow.nPeriods)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Deltat_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodDeltat
                write(TmpSTR,'(a,1pg12.4,a)')TAB//TAB//'Starting time step size: ',modflow.StressPeriodDeltat,'     '//TRIM(modflow.STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tminat_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodTminat
                write(TmpSTR,'(a,1pg12.4,a)')TAB//TAB//'Minimum time step size: ',modflow.StressPeriodTminat,'     '//TRIM(modflow.STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tmaxat_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodTmaxat
                write(TmpSTR,'(a,1pg12.4,a)')TAB//TAB//'Maximum time step size: ',modflow.StressPeriodTmaxat,'     '//TRIM(modflow.STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tadjat_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodtadjat
                write(TmpSTR,'(a,1pg12.4)')TAB//TAB//'Time step size adjustment factor: ',modflow.StressPeriodtadjat
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tcutat_CMD) /=0) then
                read(FNumMUT,*) modflow.StressPeriodTcutat
                write(TmpSTR,'(a,1pg12.4,a)')TAB//TAB//'Time step size cutting factor: ',modflow.StressPeriodTcutat
                call Msg(trim(TmpSTR))

            else
			    call ErrMsg(TAB//'Unrecognized instruction: stress period')
            end if

        end do read_StressPeriod_instructions
    end subroutine StressPeriod

    !-------------------------------------------------------------
    subroutine SWFToTecplot(Modflow,TMPLT_SWF)
        implicit none
        type(ModflowProject) Modflow
        type(TecplotDomain) TMPLT_SWF

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j

        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.'//trim(modflow.SWF.name)//'.tecplot.dat'
        
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(TMPLT_SWF.name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","'//trim(modflow.SWF.name)//' Zone","'//trim(modflow.SWF.name)//' zCell",'
        nVar=5
            
        if(allocated(Modflow.SWF.Sgcl)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.SWF.name)//' SWF-GWF connection length",'
            nVar=nVar+1
        end if
            
        if(allocated(Modflow.SWF.StartingHeads)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.SWF.name)//' Initial Depth",'
            nVar=nVar+1
        end if
        
        if(allocated(Modflow.SWF.CellArea)) then
            VarSTR=trim(VarSTR)//'"'//trim(modflow.SWF.name)//' Cell area",'
            nVar=nVar+1
        end if
                
        write(FNum,'(a)') trim(VarSTR)
          
        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(modflow.SWF.name)//'"  ,N=',TMPLT_SWF.nNodes,', E=',TMPLT_SWF.nElements,&
        ', datapacking=block, zonetype='//trim(TMPLT_SWF.elementtype)
            
        if(modflow.NodalControlVolume) then
            write(FNum,'(a)') trim(ZoneSTR) 
        else
            CellCenteredSTR=', VARLOCATION=([4'
            if(nVar.ge.5) then
                do j=5,nVar
                    if(.not. Modflow.NodalControlVolume) then  ! z Cell is not CELLCENTERED
                        write(str2,'(i2)') j
                        CellCenteredSTR=trim(CellCenteredSTR)//','//str2
                    end if
                end do
            end if
            CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'

            write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR) 
        end if
        
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (TMPLT_SWF.x(i),i=1,TMPLT_SWF.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (TMPLT_SWF.y(i),i=1,TMPLT_SWF.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (TMPLT_SWF.z(i),i=1,TMPLT_SWF.nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (Modflow.SWF.iZone(i),i=1,Modflow.SWF.nCells)
            
        write(FNum,'(a)') '# zCell i.e. cell bottom'
        write(FNum,'(5e20.12)') (Modflow.SWF.zCell(i),i=1,Modflow.SWF.nCells)
            
        if(allocated(Modflow.SWF.Sgcl)) then
            write(FNum,'(a)') '# SW-GW connection length'
            write(FNum,'(5e20.12)') (Modflow.SWF.Sgcl(i),i=1,Modflow.SWF.nCells)
        end if

        if(allocated(Modflow.SWF.StartingHeads)) then
            write(FNum,'(a)') '# Starting depth'
            write(FNum,'(5e20.12)') (Modflow.SWF.StartingHeads(i)-Modflow.SWF.ZCell(i),i=1,Modflow.SWF.nCells)
        end if

        if(allocated(Modflow.SWF.CellArea)) then
            write(FNum,'(a)') '# Cell Area'
            write(FNum,'(5e20.12)') (Modflow.SWF.CellArea(i),i=1,Modflow.SWF.nCells)
        end if
        
        do i=1,TMPLT_SWF.nElements
            if(TMPLT_SWF.nNodesPerElement==3) then ! 3-node triangle, repeat node 3 for 4-node tecplot type fequadrilateral
                write(FNum,'(8i8)') (TMPLT_SWF.iNode(j,i),j=1,3) !, TMPLT_SWF.iNode(3,i) 
            else if(TMPLT_SWF.nNodesPerElement==4) then ! 4-node quadrilateral
                if(TMPLT_SWF.iNode(4,i) > 0) then
                    write(FNum,'(8i8)') (TMPLT_SWF.iNode(j,i),j=1,4) 
                else
                    write(FNum,'(8i8)') (TMPLT_SWF.iNode(j,i),j=1,3), TMPLT_SWF.iNode(3,i) 
                end if
            else
                write(TmpSTR,'(i2)') TMPLT_SWF.nNodesPerElement
                call ErrMsg(trim(TMPLT_SWF.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if
        end do
       
        call FreeUnit(FNum)
        
    end subroutine SWFToTecplot


    !-------------------------------------------------------------
    subroutine TemplateToTecplot(Modflow,TMPLT)
        implicit none
        type(ModflowProject) Modflow
        type(TecplotDomain) TMPLT

        integer :: Fnum
        character(MAX_STR) :: FName
        integer :: i, j

        ! tecplot output file
        FName=trim(Modflow.MUTPrefix)//'o.'//trim(TMPLT.name)//'.tecplot.dat'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(TMPLT.name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","'//trim(TMPLT.name)//' Zone","'//trim(TMPLT.name)//' Element Area",'
        nVar=5

        if(allocated(TMPLT.rCircle)) then
            VarSTR=trim(VarSTR)//'"'//trim(TMPLT.name)//'Inner circle radius",'
            nVar=nVar+1
        end if
            
        write(FNum,'(a)') trim(VarSTR)


        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(TMPLT.name)//'"  ,N=',TMPLT.nNodes,', E=',TMPLT.nElements,&
        ', datapacking=block, zonetype='//trim(TMPLT.elementtype)
        
        CellCenteredSTR=', VARLOCATION=([4,5'
        if(nVar.ge.6) then
            do j=6,nVar
                write(str2,'(i2)') j
                CellCenteredSTR=trim(CellCenteredSTR)//','//str2
            end do
        end if
        CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'
        write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (TMPLT.x(i),i=1,TMPLT.nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (TMPLT.y(i),i=1,TMPLT.nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (TMPLT.z(i),i=1,TMPLT.nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (TMPLT.iZone(i),i=1,TMPLT.nElements)
            
        write(FNum,'(a)') '# element area'
        write(FNum,'(5e20.12)') (TMPLT.ElementArea(i),i=1,TMPLT.nElements)
            
        if(allocated(TMPLT.rCircle)) then
            write(FNum,'(a)') '# circle radius'
            write(FNum,'(5e20.12)') (TMPLT.rCircle(i),i=1,TMPLT.nElements)
        end if
            
        
        do i=1,TMPLT.nElements
            if(TMPLT.nNodesPerElement==3) then ! 3-node triangle
                write(FNum,'(8i8)') (TMPLT.iNode(j,i),j=1,3)
            else if(TMPLT.nNodesPerElement==4) then ! 4-node quadrilateral
                if(TMPLT.iNode(4,i) > 0) then
                    write(FNum,'(8i8)') (TMPLT.iNode(j,i),j=1,4) 
                else
                    write(FNum,'(8i8)') (TMPLT.iNode(j,i),j=1,3), TMPLT.iNode(3,i) 
                end if
            else
                write(TmpSTR,'(i2)')TMPLT.nNodesPerElement
                call ErrMsg(trim(TMPLT.name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if

        end do
       
        call FreeUnit(FNum)
        
    end subroutine TemplateToTecplot
    
    !-------------------------------------------------------------
    subroutine WriteCHDFile(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
        
        integer :: i
 	
        !------------------- CHD file
        write(modflow.iCHD,*) modflow.GWF.nCHDCells+modflow.CLN.nCHDCells+modflow.SWF.nCHDCells ! maximum number of CHD cells in any stress period 
        write(modflow.iCHD,*) modflow.GWF.nCHDCells+modflow.CLN.nCHDCells+modflow.SWF.nCHDCells ! number of CHD cells to read
            
        if(allocated(modflow.GWF.ConstantHead)) then
            do i=1,modflow.GWF.nCells
                if(bcheck(Modflow.GWF.Cell_Is(i),ConstantHead)) then
                    write(modflow.iCHD,'(i8,2x,g15.5)') i,modflow.GWF.ConstantHead(i)
                end if
            end do
        end if
            
        if(allocated(modflow.CLN.ConstantHead)) then
            do i=1,modflow.CLN.nCells
                if(bcheck(Modflow.CLN.Cell_Is(i),ConstantHead)) then
                    write(modflow.iCHD,'(i8,2x,g15.5)') Modflow.GWF.nCells+i,modflow.CLN.ConstantHead(i)
                end if
            end do
        end if
        
        if(allocated(modflow.SWF.ConstantHead)) then
            do i=1,modflow.SWF.nCells
                if(bcheck(Modflow.SWF.Cell_Is(i),ConstantHead)) then
                    write(modflow.iCHD,'(i8,2x,g15.5)') Modflow.GWF.nCells+modflow.CLN.nCells+i,modflow.SWF.ConstantHead(i)
                end if
            end do
        end if
        
    end subroutine WriteCHDFile

    !-------------------------------------------------------------
    subroutine WriteCLNFiles(Modflow,TMPLT_CLN)
        implicit none
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT_CLN
        
        
        integer :: i, j, k, i1, i2
        character(MAX_STR) :: OutputLine
        
        Modflow.CLN.NCLNGWC=Modflow.CLN.nCells  ! assume for now that all cln cells are connected to underlying gwf cells

        write(Modflow.iCLN,'(a)') '#1.    NCLN, ICLNNDS, ICLNCB,  ICLNHD,  ICLNDD,   ICLNIB,  NCLNGWC,  NCONDUITYP'
        write(OutputLine,'(8i9,a,i9)')  0, & !NCLN
                                        Modflow.CLN.nCells, &   !ICLNNDS
                                        Modflow.CLN.iCBB, &     !ICLNCB
                                        Modflow.CLN.iHDS,&      !ICLNHD
                                        Modflow.CLN.iDDN,&      !ICLNDD
                                        0,&                     !ICLNIB, if 0 CLN IBOUND array not written 
                                        Modflow.CLN.NCLNGWC,&   !NCLNGWC 
                                        Modflow.CLN.NCONDUITYP  !NCONDUITYP
        if(Modflow.CLN.NRECTYP > 0) then
            write(tmpSTR,'(a,i9)') ',        rectangular', &   !IFNO
                                        Modflow.CLN.NRECTYP  !NRECTYP 
            OutputLine=trim(OutputLine)//trim(tmpSTR)
        endif
        write(Modflow.iCLN,'(a)') trim(OutputLine)
        
        write(Modflow.iCLN,'(i9)') Modflow.CLN.njag
        write(Modflow.iCLN,'(a)') 'INTERNAL  1  (FREE)  -1  IA()'
        write(Modflow.iCLN,'(10i4)') (modflow.CLN.ia(i),i=1,modflow.CLN.nCells)
        write(Modflow.iCLN,'(a)') 'INTERNAL  1  (FREE)  -1  JA()'
        i1=1
        do i=1,modflow.CLN.nCells
            i2=i1+modflow.CLN.ia(i)-1
            write(Modflow.iCLN,*) (abs(modflow.CLN.ja(j)),j=i1,i2)
            i1=i2+1
        end do
        
        write(Modflow.iCLN,'(a)') '# IFNO,          IFTYP,        IFDIR,   FLENG,         FELEV,         FANGLE,     IFLIN, ICCWADI'
        do i=1,modflow.CLN.nCells
            write(Modflow.iCLN,'(i5,5(1pG15.5),2i5)') i, & !IFNO
            modflow.CLN.IZone(i), & !IFTYP
            modflow.CLN.Direction(modflow.CLN.IZone(i)), & !IFDIR
            modflow.CLN.Length(i), & !FLENG
            modflow.CLN.LowestElevation(i), & !FELEV
            modflow.CLN.SlopeAngle(i), & !FANGLE
            Modflow.CLN.FlowTreatment(modflow.CLN.IZone(i)), & !IFLIN
            0   ! ICCWADI 
        end do

        write(Modflow.iCLN,'(a)') '# IFNOD IGWNOD IFCON    FSKIN      FLENGW      FANISO  ICGWADI'
        do i=1,modflow.CLN.nCells
            write(Modflow.iCLN,'(3i5,3(1pG15.5),i5)') i, & !IFNO
            i, & !IGWNOD
            3, & !IFCON
            1.e-20, & !FSKIN
            modflow.CLN.Length(i), & !FLENGW
            1.00, & !FANISO
            0   ! ICGWADI 
        end do


        if(Modflow.CLN.NCONDUITYP>0) then
            write(Modflow.iCLN,'(a)') '# ICONDUITYP   FRAD         CONDUITK'
            do i=1,modflow.CLN.NZones
                if(modflow.CLN.Geometry(i) == 1) then 
                    write(Modflow.iCLN,'(i5,3x,2(1pG15.5))') i, modflow.CLN.CircularRadius(i), modflow.CLN.LongitudinalK(i)
                endif
            end do
        endif
        
        if(Modflow.CLN.NRECTYP>0) then
            write(Modflow.iCLN,'(a)') '# IRECTYP    FLENGTH        FHEIGHT       CONDUITK'
            do i=1,modflow.CLN.NZones
                if(modflow.CLN.Geometry(i) == 2) then 
                    write(Modflow.iCLN,'(i5,3x,3(1pG15.5))') i, modflow.CLN.RectangularWidth(i),modflow.CLN.RectangularHeight(i), modflow.CLN.LongitudinalK(i)
                endif
            end do
        endif
        
        ! fix here YJP
        if(.not. allocated(modflow.CLN.ibound)) then ! Assume ibound is 1 for now (i.e. all nodes have variable head)'
            allocate(modflow.CLN.ibound(modflow.CLN.nCells),stat=ialloc)
            call AllocChk(ialloc,'CLN Cell ibound array')            
            modflow.CLN.ibound(:)=1
        end if
        do i=1,Modflow.CLN.nCells
            if(bcheck(modflow.CLN.Cell_is(i),inactive)) modflow.CLN.ibound(i)=0
        enddo
        write(Modflow.iCLN,'(a)') 'INTERNAL  1  (FREE)  -1  IBOUND'
        write(Modflow.iCLN,'(10i3)') (modflow.CLN.ibound(k),k=1,Modflow.CLN.nCells)
        
        if(.not. allocated(modflow.CLN.StartingHeads)) then ! Assume equal to zcell i.e. depth zero + 1e-4'
            allocate(modflow.CLN.StartingHeads(modflow.CLN.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell starting heads array')            
            modflow.CLN.StartingHeads(:)=modflow.CLN.zcell(:)+1.0e-4
        end if
        write(Modflow.iCLN,'(a)') 'INTERNAL  1.000000e+000  (FREE)  -1  Starting Heads()'
        write(Modflow.iCLN,'(5(1ES20.8))') (modflow.CLN.StartingHeads(i),i=1,modflow.CLN.nCells)
        
        !------------------- write CLN_GSF file
        write(Modflow.iCLN_GSF,'(a)') trim(TMPLT_CLN.meshtype)
        write(Modflow.iCLN_GSF,*) Modflow.CLN.nCells, Modflow.CLN.nLayers, Modflow.CLN.iz, Modflow.CLN.ic
        write(Modflow.iCLN_GSF,*) Modflow.CLN.nNodes
        write(Modflow.iCLN_GSF,*) (TMPLT_CLN.x(i),TMPLT_CLN.y(i),TMPLT_CLN.z(i),i=1,Modflow.CLN.nNodes)
        do i=1,Modflow.CLN.nCells
            write(Modflow.iCLN_GSF,'(i10,2x,3(1pg15.5),2x,2i10,10i10)') i,Modflow.CLN.xCell(i),Modflow.CLN.yCell(i),Modflow.CLN.zCell(i),Modflow.CLN.iLayer(i),Modflow.CLN.nNodesPerCell,(Modflow.CLN.iNode(j,i),j=1,Modflow.CLN.nNodesPerCell)
        end do
 
    end subroutine WriteCLNFiles
    
    !-------------------------------------------------------------
    subroutine WriteDRNFile(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
        
        integer :: i
 	
        !------------------- DRN file
        write(modflow.iDRN,*) modflow.GWF.nDRNCells+modflow.CLN.nDRNCells+modflow.SWF.nDRNCells ! maximum number of DRN cells in any stress period 
        write(modflow.iDRN,*) modflow.GWF.nDRNCells+modflow.CLN.nDRNCells+modflow.SWF.nDRNCells ! number of DRN cells to read
            
        if(allocated(modflow.GWF.DrainConductance)) then
            do i=1,modflow.GWF.nCells
                if(bcheck(Modflow.GWF.Cell_Is(i),Drain)) then
                    write(modflow.iDRN,'(i8,2x,2g15.5)') i,modflow.GWF.DrainElevation(i),modflow.GWF.DrainConductance(i)
                end if
            end do
        end if
            
        if(allocated(modflow.CLN.DrainConductance)) then
            do i=1,modflow.CLN.nCells
                if(bcheck(Modflow.CLN.Cell_Is(i),Drain)) then
                    write(modflow.iDRN,'(i8,2x,2g15.5)') Modflow.GWF.nCells+i,modflow.CLN.DrainElevation(i),modflow.CLN.DrainConductance(i)
                end if
            end do
        end if
        
        if(allocated(modflow.SWF.DrainConductance)) then
            do i=1,modflow.SWF.nCells
                if(bcheck(Modflow.SWF.Cell_Is(i),Drain)) then
                    write(modflow.iDRN,'(i8,2x,2g15.5)') Modflow.GWF.nCells+modflow.CLN.nCells+i,modflow.SWF.DrainElevation(i),modflow.SWF.DrainConductance(i)
                end if
            end do
        end if
        

        
    end subroutine WriteDRNFile

    !-------------------------------------------------------------
    subroutine WriteGWFFiles(Modflow,TMPLT_GWF)
        implicit none
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT_GWF

        integer :: i, j, k, i1, i2, nStrt,nEnd
        
        !------------------- BAS6 file
        ! Hardwired for unstructured, free format and solving Richard's equation when built from scratch
        write(Modflow.iBAS6,'(a)') 'UNSTRUCTURED FREE RICHARDS'
        modflow.unstructured=.true.
        IVSD=-1  ! indicates a vertically stacked grid.  Read horizonatl cell area for top layer only.    
        modflow.free=.true.
        modflow.richards=.true.

        if(.not. allocated(modflow.GWF.ibound)) then ! Assume ibound is 1 for now (i.e. all nodes have variable head)'
            allocate(modflow.GWF.ibound(modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell ibound array')            
            modflow.GWF.ibound(:)=1
        end if
        do i=1,Modflow.GWF.nCells
            if(bcheck(modflow.GWF.Cell_is(i),inactive)) modflow.GWF.ibound(i)=0
        enddo
        !write(Modflow.iBAS6,'(a)') 'CONSTANT   1                               IBOUND'
        nStrt=1
        do i=1,Modflow.GWF.nLayers
            write(TmpSTR,'(i5)') i
            write(Modflow.iBAS6,'(a)') 'INTERNAL  1  (FREE)  -1  IBOUND Layer '//trim(TmpSTR)
            nEnd = nStrt + modflow.GWF.nodelay-1
            write(Modflow.iBAS6,'(10i3)') (modflow.GWF.ibound(k),k=nStrt,nEnd)
            nStrt=nEnd+1
        end do
        
        write(Modflow.iBAS6,'(10ES20.8)') modflow.GWF.StartingHeads(1)  ! hnoflo, head value to be printed for no-flow cells

        if(.not. allocated(modflow.GWF.StartingHeads)) then ! Assume 2.78 m for abdul for now'
            allocate(modflow.GWF.StartingHeads(modflow.GWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell starting heads array')            
            modflow.GWF.StartingHeads(:)=1.5d0 !2.78d0
        end if
        nStrt=1
        do i=1,Modflow.GWF.nLayers
            write(TmpSTR,'(i5)') i
            write(Modflow.iBAS6,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Starting Heads Layer '//trim(TmpSTR)
            nEnd = nStrt + modflow.GWF.nodelay-1
            write(Modflow.iBAS6,'(10G12.5)') (modflow.GWF.StartingHeads(k),k=nStrt,nEnd)
            nStrt=nEnd+1
        end do


        !------------------- DISU file

        write(Modflow.iDISU,'(a)') '#1.   NODES    NLAY     NJAG     IVSD     NPER    ITMUNI   LENUNI   IDSYMRD'
        write(Modflow.iDISU,'(10i9)') Modflow.GWF.nCells, Modflow.GWF.nLayers, Modflow.GWF.njag, IVSD, Modflow.nPeriods, &
            Modflow.TimeUnits, Modflow.LengthUnits , 0
            
        write(Modflow.iDISU,'(10i4)') (modflow.GWF.Laybcd(i),i=1,Modflow.GWF.nLayers)
            
        write(Modflow.iDISU,'(a,i10,a)') 'CONSTANT ',modflow.GWF.nodelay,'    NODELAY'
            
            
        !write(FNum,'(a)') 'OPEN/CLOSE OpenClose_CellTopElev.in   1.000000e+00  (FREE)  1   Top elevation'
        !FNameOpenClose='OpenClose_CellTopElev.in'
        !call OpenAscii(FNumOpenClose,FNameOpenClose)
        !nStrt=1
        !do k = 1,modflow.GWF.nLayers
        !    nEnd = nStrt + modflow.GWF.nodelay
        !    write(FNumOpenClose,*) (modflow.GWF.top(i),i=nStrt,nEnd)
        !end do
        !close(FNumOpenClose)
            
        nStrt=1
        do i=1,Modflow.GWF.nLayers
            write(TmpSTR,'(i5)') i
            write(Modflow.iDISU,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Top elevation Layer '//trim(TmpSTR)
            nEnd = nStrt + modflow.GWF.nodelay-1
            write(Modflow.iDISU,'(10G16.5)') (modflow.GWF.top(k),k=nStrt,nEnd)
            nStrt=nEnd+1
        end do
            
        nStrt=1
        do i=1,Modflow.GWF.nLayers
            write(TmpSTR,'(i5)') i
            write(Modflow.iDISU,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Bottom elevation Layer '//trim(TmpSTR)
            nEnd = nStrt + modflow.GWF.nodelay-1
            write(Modflow.iDISU,'(10G16.5)') (modflow.GWF.bottom(k),k=nStrt,nEnd)
            nStrt=nEnd+1
        end do
            
        nStrt=1
        if(ivsd == -1) then
            do i=1,1
                write(TmpSTR,'(i5)') i
                write(Modflow.iDISU,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Horizontal Area Layer '//trim(TmpSTR)
                nEnd = nStrt + modflow.GWF.nodelay-1
                write(Modflow.iDISU,'(10G16.5)') (modflow.GWF.CellArea(k),k=nStrt,nEnd)
                nStrt=nEnd+1
            end do
        else
            do i=1,Modflow.GWF.nLayers
                write(TmpSTR,'(i5)') i
                write(Modflow.iDISU,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Horizontal Area Layer '//trim(TmpSTR)
                nEnd = nStrt + modflow.GWF.nodelay-1
                write(Modflow.iDISU,'(10G16.5)') (modflow.GWF.CellArea(k),k=nStrt,nEnd)
                nStrt=nEnd+1
            end do
        end if
        
        write(Modflow.iDISU,'(a)') 'INTERNAL  1  (FREE)  -1  IA()'
        write(Modflow.iDISU,'(10i4)') (modflow.GWF.ia(i),i=1,modflow.GWF.nCells)
        write(Modflow.iDISU,'(a)') 'INTERNAL  1  (FREE)  -1  JA()'
        i1=1
        do i=1,modflow.GWF.nCells
            i2=i1+modflow.GWF.ia(i)-1
            write(Modflow.iDISU,*) (modflow.GWF.ja(j),j=i1,i2)
            i1=i2+1
        end do
        
        write(Modflow.iDISU,'(a)') 'INTERNAL  1  (FREE)  -1  Connection Length CLN()'
        i1=1
        do i=1,modflow.GWF.nCells
            i2=i1+modflow.GWF.ia(i)-1
            write(Modflow.iDISU,*) (modflow.GWF.ConnectionLength(j),j=i1,i2)
            i1=i2+1
        end do
        
        write(Modflow.iDISU,'(a)') 'INTERNAL  1  (FREE)  -1  Perpendicular Area FAHL()'
        i1=1
        do i=1,modflow.GWF.nCells
            i2=i1+modflow.GWF.ia(i)-1
            write(Modflow.iDISU,*) (modflow.GWF.PerpendicularArea(j),j=i1,i2)
            i1=i2+1
        end do
        
        do i=1,Modflow.nPeriods
            write(Modflow.iDISU,*) modflow.StressPeriodDuration(i), modflow.StressPeriodnTsteps(i), modflow.StressPeriodnTstepMult(i), modflow.StressPeriodType(i)
        end do


        !------------------- LPF file
        write(Modflow.iLPF,'(a)') '#1a. ILPFCB      HDRY         NPLPF   IKCFLAG     Options'
        write(Modflow.iLPF,'(i9,1pG15.5,2i9,a)') Modflow.GWF.iCBB, -1.000000e+30, 0, 0, '        CONSTANTCV NOVFC'

        ! Layer type: Assume type 4 (i.e. convertible, with transmissivity computed using upstream water-table depth
        if(.not. allocated(modflow.GWF.LayTyp)) then 
            allocate(modflow.GWF.LayTyp(modflow.GWF.nLayers),stat=ialloc)
            call AllocChk(ialloc,'Layer type array')            
            modflow.GWF.LayTyp(:) = 4
        end if
        write(Modflow.iLPF,'(40i2)') (modflow.GWF.LayTyp(k),k=1,modflow.GWF.nlayers)

        ! Layer average: Assume 0, harmonic mean 
        if(.not. allocated(modflow.GWF.LayAvg)) then 
            allocate(modflow.GWF.LayAvg(modflow.GWF.nLayers),stat=ialloc)
            call AllocChk(ialloc,'Layer averaging method array')            
            modflow.GWF.LayAvg(:) = 0
        end if
        write(Modflow.iLPF,'(40i2)') (modflow.GWF.LayAvg(k),k=1,modflow.GWF.nlayers)

        ! Chani: Assume 1.0, If Chani is greater than 0, then Chani is the horizontal anisotropy for the entire layer.
        if(.not. allocated(modflow.GWF.Chani)) then 
            allocate(modflow.GWF.Chani(modflow.GWF.nLayers),stat=ialloc)
            call AllocChk(ialloc,'Layer horizontal anisotropy array')            
            modflow.GWF.Chani(:) = 1.0d0
        end if
        write(Modflow.iLPF,'(10G12.5)') (modflow.GWF.Chani(k),k=1,modflow.GWF.nlayers)

        ! LayVka: Assume 0, indicates VKA is vertical hydraulic conductivity
        if(.not. allocated(modflow.GWF.LayVka)) then 
            allocate(modflow.GWF.LayVka(modflow.GWF.nLayers),stat=ialloc)
            call AllocChk(ialloc,'Layer vertical K array')            
            modflow.GWF.LayVka(:) = 0
        end if
        write(Modflow.iLPF,'(40i2)') (modflow.GWF.LayVka(k),k=1,modflow.GWF.nlayers)

        ! LayWet: Assume 0, indicates wetting is inactive
        if(.not. allocated(modflow.GWF.LayWet)) then 
            allocate(modflow.GWF.LayWet(modflow.GWF.nLayers),stat=ialloc)
            call AllocChk(ialloc,'Layer vertical K array')            
            modflow.GWF.LayWet(:) = 0
        end if
        write(Modflow.iLPF,'(40i2)') (modflow.GWF.LayWet(k),k=1,modflow.GWF.nlayers)
        
        nStrt=1
        do i=1,Modflow.GWF.nLayers
            nEnd = nStrt + modflow.GWF.nodelay-1

            if(.not. allocated(modflow.GWF.Kh)) then ! Assume 1e-5 m/s (Borden Sand from Abdul problem)'
                allocate(modflow.GWF.Kh(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Kh array')            
                modflow.GWF.Kh(:)=31.536D0
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Kh '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Kh(k),k=nStrt,nEnd)


            if(.not. allocated(modflow.GWF.Kv)) then ! Assume 1e-5 m/s (Borden Sand from Abdul problem)'
                allocate(modflow.GWF.Kv(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Kv array')            
                modflow.GWF.Kv(:)=31.536D0
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Kv '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Kv(k),k=nStrt,nEnd)

            if(.not. allocated(modflow.GWF.Ss)) then ! Assume 1.2e-7 (Borden Sand from Abdul problem)'
                allocate(modflow.GWF.Ss(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Ss array')            
                modflow.GWF.Ss(:)=1.0D-5
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Ss '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Ss(k),k=nStrt,nEnd)

            if(.not. allocated(modflow.GWF.Sy)) then ! Assume 0.34 (Borden Sand from Abdul problem)''
                allocate(modflow.GWF.Sy(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Sy array')            
                modflow.GWF.Sy(:)=0.01D0
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Sy '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Sy(k),k=nStrt,nEnd)

            if(.not. allocated(modflow.GWF.Alpha)) then ! Assume 1.8 (Borden Sand from InHM PM.DBS file)''
                allocate(modflow.GWF.Alpha(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Alpha array')            
                modflow.GWF.Alpha(:)=3.34D-2
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Alpha '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Alpha(k),k=nStrt,nEnd)

            if(.not. allocated(modflow.GWF.Beta)) then ! Assume 5.8 (Borden Sand from InHM PM.DBS file)''
                allocate(modflow.GWF.Beta(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Beta array')            
                modflow.GWF.Beta(:)=1.982D0
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Beta '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Beta(k),k=nStrt,nEnd)

            if(.not. allocated(modflow.GWF.Sr)) then ! Assume 0.18 (Borden Sand from Abdul mprops S-R table)''
                allocate(modflow.GWF.Sr(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Sr array')            
                modflow.GWF.Sr(:)=2.771D-1
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Sr '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Sr(k),k=nStrt,nEnd)


            if(.not. allocated(modflow.GWF.Brooks)) then ! Assume -1.0 (Brooks from hillslope example problem)''
                allocate(modflow.GWF.Brooks(modflow.GWF.nCells),stat=ialloc)
                call AllocChk(ialloc,'Cell Brooks array')            
                modflow.GWF.Brooks(:)=5.037D0
            end if
            write(TmpSTR,'(i5)') i
            write(Modflow.iLPF,'(a)') 'INTERNAL  1.000000e+00  (FREE)  -1  Brooks '//trim(TmpSTR)
            write(Modflow.iLPF,'(10G16.5)') (modflow.GWF.Brooks(k),k=nStrt,nEnd)

            nStrt=nEnd+1
        end do
        
        !------------------- SMS file
        write(TmpSTR,'(i4)') iSMSParameterSet
        write(Modflow.iSMS,'(a)') '# SMS parameter set from record '//trim(TmpSTR)//', '//trim(SMS_Name(iSMSParameterSet))
        write(Modflow.iSMS,'(a)') '#1b.   HCLOSE         HICLOSE      MXITER     ITER1     IPRSMS   NONLINMETH LINMETH   Options...'
        write(Modflow.iSMS,'(2(1pe15.3),5i10,6x,a)') SMS_HCLOSE(iSMSParameterSet),  &
            SMS_HICLOSE(iSMSParameterSet), &
            SMS_MXITER(iSMSParameterSet), &
            SMS_ITER1(iSMSParameterSet), &
            SMS_IPRSMS(iSMSParameterSet), &
            SMS_NONLINMETH(iSMSParameterSet), &
            SMS_LINMETH(iSMSParameterSet), &
            SMS_Options(iSMSParameterSet)
        write(Modflow.iSMS,'(a)') '#2.  THETA    AKAPPA     GAMMA   AMOMENTUM  NUMTRACK   BTOL     BREDUC    RESLIM'
        write(Modflow.iSMS,'(4f10.3,i10,3f10.3)') SMS_THETA(iSMSParameterSet),  &
            SMS_KAPPA(iSMSParameterSet), &
            SMS_GAMMA(iSMSParameterSet), &
            SMS_AMOMENTUM(iSMSParameterSet), &
            SMS_NUMTRACK(iSMSParameterSet), &
            SMS_BTOL(iSMSParameterSet), &
            SMS_BREDUC(iSMSParameterSet), &
            SMS_RES_LIM(iSMSParameterSet)
        write(Modflow.iSMS,'(a)') '#3.    IACL     NORDER     LEVEL    NORTH     IREDSYS  RRCTOL    IDROPTOL    EPSRN'
        write(Modflow.iSMS,'(5i10,f10.3,i10,1pe15.3)') SMS_IACL(iSMSParameterSet),  &
            SMS_NORDER(iSMSParameterSet), &
            SMS_LEVEL(iSMSParameterSet), &
            SMS_NORTH(iSMSParameterSet), &
            SMS_IREDSYS(iSMSParameterSet), &
            SMS_RRCTOL(iSMSParameterSet), &
            SMS_IDROPTOL(iSMSParameterSet), &
            SMS_EPSRN(iSMSParameterSet)

        !------------------- OC file
        write(Modflow.iOC,'(a,i5)') 'ATSA NPTIMES',modflow.nOutputTimes
        write(Modflow.iOC,*) (modflow.OutputTimes(i),i=1,modflow.nOutputTimes)
        write(Modflow.iOC,'(a,i5)') 'HEAD SAVE UNIT ',modflow.GWF.iHDS
        write(Modflow.iOC,'(a,i5)') 'HEAD PRINT FORMAT 0'
        write(Modflow.iOC,'(a,i5)') 'DRAWDOWN SAVE UNIT ',modflow.GWF.iDDN
        write(Modflow.iOC,'(a,i5)') 'DRAWDOWN PRINT FORMAT 0'
        do i=1,modflow.nPeriods
            write(Modflow.iOC,'(a,i5)') 'PERIOD ',i
            write(Modflow.iOC,'(a,1pg12.4)') '    DELTAT ', modflow.StressPeriodDeltat
            write(Modflow.iOC,'(a,1pg12.4)') '    TMINAT ', modflow.StressPeriodTminat
            write(Modflow.iOC,'(a,1pg12.4)') '    TMAXAT ', modflow.StressPeriodTmaxat
            write(Modflow.iOC,'(a,1pg12.4)') '    TADJAT ', modflow.StressPeriodTadjat
            write(Modflow.iOC,'(a,1pg12.4)') '    TCUTAT ', modflow.StressPeriodTcutat
            write(Modflow.iOC,'(a)') '        SAVE HEAD'
            write(Modflow.iOC,'(a)') '        PRINT HEAD'
            write(Modflow.iOC,'(a)') '        SAVE DRAWDOWN'
            write(Modflow.iOC,'(a)') '        SAVE BUDGET'
            write(Modflow.iOC,'(a)') '        PRINT BUDGET'
        end do

        
        
        if(modflow.NodalControlVolume) then
            !------------------- GSF file for nodal control volume
            write(Modflow.iGSF,'(a)') 'UNSTRUCTURED(NODALCONTROLVOLUME)'
            write(Modflow.iGSF,*) TMPLT_GWF.nElements, Modflow.GWF.nLayers, Modflow.GWF.iz, Modflow.GWF.ic
            write(Modflow.iGSF,*) Modflow.GWF.nNodes
            write(Modflow.iGSF,*) (TMPLT_GWF.x(i),TMPLT_GWF.y(i),TMPLT_GWF.z(i),i=1,TMPLT_GWF.nNodes)
            do i=1,TMPLT_GWF.nElements
                write(Modflow.iGSF,'(12i10)') i,TMPLT_GWF.nNodesPerElement,(TMPLT_GWF.iNode(j,i),j=1,TMPLT_GWF.nNodesPerElement)
            end do
            do i=1,Modflow.GWF.nCells
                write(Modflow.iGSF,'(i10,3(1pg15.5),i10)') i,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i)
            end do
        else
            !------------------- GSF file for elemental control volume
            write(Modflow.iGSF,'(a)') trim(TMPLT_GWF.meshtype)
            write(Modflow.iGSF,*) Modflow.GWF.nCells, Modflow.GWF.nLayers, Modflow.GWF.iz, Modflow.GWF.ic
            write(Modflow.iGSF,*) Modflow.GWF.nNodes
            write(Modflow.iGSF,*) (TMPLT_GWF.x(i),TMPLT_GWF.y(i),TMPLT_GWF.z(i),i=1,TMPLT_GWF.nNodes)
            do i=1,Modflow.GWF.nCells
                write(Modflow.iGSF,'(i10,2x,3(1pg15.5),2x,2i10,10i10)') i,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i),Modflow.GWF.nNodesPerCell,(Modflow.GWF.iNode(j,i),j=1,Modflow.GWF.nNodesPerCell)
            end do
        end if
        

        return
    end subroutine WriteGWFFiles
    
    !-------------------------------------------------------------
    subroutine WriteSWFFiles(Modflow,TMPLT_SWF)
        implicit none
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT_SWF

        integer :: i, j, i1, i2,k
        
        write(Modflow.iSWF,'(a)') '#1. NSWFNDS  NJA_SWF  NSWFGWC   NSWFTYP  ISWFCB  ISWFHD   ISWFDD    ISWFIB'
        write(Modflow.iSWF,'(10i9)') Modflow.SWF.nCells, & ! NSWFNDS 
            Modflow.SWF.njag, & ! NJA_SWF
            Modflow.SWF.nCells, & ! NSWFGWC, assume for now all swf cells are connected to underlying gwf
            modflow.SWF.nZones, & ! NSWFTYP
            Modflow.SWF.iCBB, & ! ISWFCB
            Modflow.SWF.iHDS, & ! ISWFHD
            Modflow.SWF.iDDN, & ! ISWFDD
            0 ! ISWFIB
        write(Modflow.iSWF,'(a)') 'INTERNAL  1  (FREE)  -1  IA()'
        write(Modflow.iSWF,'(10i4)') (modflow.SWF.ia(i),i=1,modflow.SWF.nCells)
        write(Modflow.iSWF,'(a)') 'INTERNAL  1  (FREE)  -1  JA()'
        i1=1
        do i=1,modflow.SWF.nCells
            i2=i1+modflow.SWF.ia(i)-1
            write(Modflow.iSWF,*) (modflow.SWF.ja(j),j=i1,i2)
            i1=i2+1
        end do
        
        write(Modflow.iSWF,'(a)') '# IFNO IFTYP   FAREA          FELEV      ISSWADI'
        do i=1,modflow.SWF.nCells
            write(Modflow.iSWF,'(2i9,2(1pG15.5),i9)') i, 1, modflow.SWF.CellArea(i), modflow.SWF.zCell(i),0
        end do

        write(Modflow.iSWF,'(a)') '# IFNO IFGWNO  IFCON     SGCL        SGCAREA      ISGWADI'
        do i=1,modflow.SWF.nCells
!            write(Modflow.iSWF,'(2i5,3x,i5,2(1pG15.5),i5)') i, i, 1, modflow.SWF.sgcl(i), modflow.SWF.CellArea(i), 0
            write(Modflow.iSWF,'(2i9,3x,i9,2(1pG15.5),i9)') i, i, 1, modflow.SWF.sgcl(i), modflow.SWF.CellArea(i), 0
        end do

        write(Modflow.iSWF,'(a)') '# ISWFTYP      SMANN          SWFH1          SWFH2'
        do i=1,modflow.SWF.nZones
            write(Modflow.iSWF,'(i5,3x,3(1pG15.5))') i,  modflow.swf.manning(i),  modflow.swf.H1DepthForSmoothing(i),  modflow.swf.H2DepthForSmoothing(i)
        end do
        
        write(Modflow.iSWF,'(a)') 'INTERNAL  1  (FREE)  -1  Connection Length CLN()'
        i1=1
        do i=1,modflow.SWF.nCells
            i2=i1+modflow.SWF.ia(i)-1
            write(Modflow.iSWF,*) (modflow.SWF.ConnectionLength(j),j=i1,i2)
            i1=i2+1
        end do
        
        write(Modflow.iSWF,'(a)') 'INTERNAL  1  (FREE)  -1  Perpendicular Area FAHL()'
        i1=1
        do i=1,modflow.SWF.nCells
            i2=i1+modflow.SWF.ia(i)-1
            write(Modflow.iSWF,*) (modflow.SWF.PerpendicularArea(j),j=i1,i2)
            i1=i2+1
        end do

        ! fix here YJP
        if(.not. allocated(modflow.SWF.ibound)) then ! Assume ibound is 1 for now (i.e. all nodes have variable head)'
            allocate(modflow.SWF.ibound(modflow.SWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell ibound array')            
            modflow.SWF.ibound(:)=1
        end if
        do i=1,Modflow.SWF.nCells
            if(bcheck(modflow.SWF.Cell_is(i),inactive)) modflow.SWF.ibound(i)=0
        enddo
        write(Modflow.iSWF,'(a)') 'INTERNAL  1  (FREE)  -1  IBOUND'
        write(Modflow.iSWF,'(10i3)') (modflow.SWF.ibound(k),k=1,Modflow.SWF.nCells)
        
        if(.not. allocated(modflow.SWF.StartingHeads)) then ! Assume equal to zcell i.e. depth zero + 1e-4'
            allocate(modflow.SWF.StartingHeads(modflow.SWF.nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell starting heads array')            
            modflow.SWF.StartingHeads(:)=modflow.SWF.zcell(:)+1.0e-4
        end if
        write(Modflow.iSWF,'(a)') 'INTERNAL  1.000000e+000  (FREE)  -1  Starting Heads()'
        write(Modflow.iSWF,'(5(1ES20.8))') (modflow.SWF.StartingHeads(i),i=1,modflow.SWF.nCells)
       

        

        
        !------------------- SWBC file
        if(allocated(modflow.SWF.CriticalDepthLength)) then
            write(Modflow.iSWBC,*) modflow.swf.nSWBCCells, modflow.swf.iCBB  ! set nrchop default to 4
            write(Modflow.iSWBC,*) 1  ! reuse bc data from last stress period if negative
            do i=1,modflow.SWF.nCells
                if(bcheck(modflow.SWF.Cell_Is(i),CriticalDepth)) then
                    write(Modflow.iSWBC,'(5(1pg15.5))') i+modflow.GWF.nCells+modflow.CLN.nCells, modflow.SWF.CriticalDepthLength(i)
                end if
            end do
        end if
        
        if(modflow.NodalControlVolume) then
            !------------------- write non-standard SWF_GSF file
            write(Modflow.iSWF_GSF,'(a)') 'UNSTRUCTURED(NODALCONTROLVOLUME)'
            write(Modflow.iSWF_GSF,*) TMPLT_SWF.nElements, Modflow.SWF.nLayers, Modflow.SWF.iz, Modflow.SWF.ic
            write(Modflow.iSWF_GSF,*) Modflow.SWF.nNodes
            write(Modflow.iSWF_GSF,*) (TMPLT_SWF.x(i),TMPLT_SWF.y(i),TMPLT_SWF.z(i),i=1,Modflow.SWF.nNodes)
            do i=1,TMPLT_SWF.nElements
                write(Modflow.iSWF_GSF,'(12i10)') i,TMPLT_SWF.nNodesPerElement,(TMPLT_SWF.iNode(j,i),j=1,TMPLT_SWF.nNodesPerElement)
            end do
            do i=1,Modflow.SWF.nCells
                write(Modflow.iSWF_GSF,'(i10,3(1pg15.5),i10)') i,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i)
            end do

        else 
            !------------------- write SWF_GSF file
            write(Modflow.iSWF_GSF,'(a)') trim(Modflow.SWF.meshtype)
            write(Modflow.iSWF_GSF,*) Modflow.SWF.nCells, Modflow.SWF.nLayers, Modflow.SWF.iz, Modflow.SWF.ic
            write(Modflow.iSWF_GSF,*) Modflow.SWF.nNodes
            write(Modflow.iSWF_GSF,*) (TMPLT_SWF.x(i),TMPLT_SWF.y(i),TMPLT_SWF.z(i),i=1,Modflow.SWF.nNodes)
            do i=1,Modflow.SWF.nCells
                write(Modflow.iSWF_GSF,'(i10,2x,3(1pg15.5),2x,2i10,10i10)') i,Modflow.SWF.xCell(i),Modflow.SWF.yCell(i),Modflow.SWF.zCell(i),Modflow.SWF.iLayer(i),Modflow.SWF.nNodesPerCell,(Modflow.SWF.iNode(j,i),j=1,Modflow.SWF.nNodesPerCell)
            end do
        end if


        return
    end subroutine WriteSWFFiles
    
    !-------------------------------------------------------------
    subroutine WriteVolumeBudgetToTecplot(Modflow)
        implicit none

        type (ModflowProject) Modflow
        
        integer :: i

        integer :: Fnum
        integer :: FnumTecplot
        character(MAX_STR) :: FNameTecplot
        
        
        character(20) :: Varname(100)
        real(dr) :: VarNumRate(100)
        Real(dr) :: VarNumCumulative(100)
        logical :: DoVars
        integer :: bline
        logical :: InSection
        real(dr) :: TotalTime
        real(dr) :: dum1, dum2, dum3, dum4
        
        character(4000) :: var_line
        character(4000) :: output_line
        character(4000) :: line
        
        integer :: len


        

        FNameTecplot=trim(Modflow.MUTPrefix)//'o.'//trim(Modflow.Prefix)//'.VolumeBudget.tecplot.dat'
        call OpenAscii(FNumTecplot,FNameTecplot)
        call Msg( 'To File: '//trim(FNameTecplot))

        write(FNumTecplot,*) 'Title = "Modflow Volume Budget"'

        DoVars=.true.
        
        FNum=Modflow.iLIST
        rewind(FNUM)

        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) return
            
            if(index(line,'MODEL TIME UNIT IS').gt.0) then
                l1=index(line,'MODEL TIME UNIT IS')
                Modflow.STR_TimeUnit=line(l1+19:)
                var_line='VARIABLES = "TOTAL TIME'//'('//trim(adjustl(Modflow.STR_TimeUnit))//')",'

                continue
                
            else if(index(line,'MODEL LENGTH UNIT IS').gt.0) then
                l1=index(line,'MODEL LENGTH UNIT IS')
                Modflow.STR_LengthUnit=line(l1+21:)
                

                continue
            else if(index(line,'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF TIME STEP').gt.0) then 
                bline=0  
                InSection=.true.
                
                do  ! find start of budget data                 
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                
                    if(index(line,'           ---                                      ---').gt.0) exit
                end do
                
                do  ! read IN section
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'OUT:') > 0) exit
                                            
                    if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)='IN_'//trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        end if
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                                  
                    end if

                end do

                do  ! read OUT section
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'TOTAL OUT =') > 0) then
                        backspace(FNum)
                        exit
                    end if
                                            
                    if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)='OUT_'//trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        end if
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                                  
                    end if

                end do
                
                do  ! read to the end of the budget data
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'TOTAL TIME').gt.0) then
                        l1=index(line,'TOTAL TIME')
                        if(index(Modflow.STR_TimeUnit,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(Modflow.STR_TimeUnit,'YEARS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, dum4, TotalTime
                        end if
                        exit
                        
                    else if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)=trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        end if
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                    
                     end if
                end do
             
                if(DoVars) then
                    l1=len_trim(var_line)
                    write(FNumTecplot,'(a)') var_line(:l1-1)
                    
                    l1 = GetCurrentDirectory( len(CurrentDir), CurrentDir )
                    l2=len_trim(CurrentDir)-1
                    
                    l1=l2
                    
                    ! check for last path seperator
                    do 
                        if(CurrentDir(l1:l1) .ne. '/' .and. CurrentDir(l1:l1) .ne. '\') then
                            l1=l1-1
                        else
                            l1=l1+1
                            exit
                        end if    
                    end do

                    write(output_line,'(a)')  'zone t="'//CurrentDir(l1:l2)//'"'
 
                    TMPStr=', AUXDATA TimeUnits = "'//trim(Modflow.STR_TimeUnit)//'"'
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(a)')	TMPStr                 

                    TMPStr=', AUXDATA LengthUnits = "'//trim(Modflow.STR_LengthUnit)//'"'
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(a)')	TMPStr                 
                    
                    write(FNumTecplot,'(a)') output_line 

                    
                    DoVars=.false.
                end if
                
                write(output_line,'(1pg20.7)') TotalTime
                do i=1,bline
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(1pg20.7)')	VarNumRate(i)
                end do

                write(FNumTecplot,'(a)') output_line

            end if
        end do 
    end subroutine WriteVolumeBudgetToTecplot
    
    !!-------------------------------------------------------------
    !subroutine WriteWELFile(Modflow)
    !    implicit none
    !    
    !    type (ModflowProject) Modflow
    !    
    !    integer :: i
 	  !
    !    !------------------- WEL file
    !    write(modflow.iWEL,*) modflow.GWF.nWELCells+modflow.CLN.nWELCells+modflow.SWF.nWELCells, modflow.iCBB    ! maximum number of WEL cells in any stress period 
    !        
    !    if(allocated(modflow.GWF.PumpingRate)) then
    !        do i=1,modflow.GWF.nCells
    !            if(bcheck(Modflow.GWF.Cell_Is(i),Well)) then
    !                write(modflow.iWEL,'(i8,2x,g15.5)') i,modflow.GWF.PumpingRate(i),0
    !            end if
    !        end do
    !    end if
    !        
    !    if(allocated(modflow.CLN.PumpingRate)) then
    !        do i=1,modflow.CLN.nCells
    !            if(bcheck(Modflow.CLN.Cell_Is(i),Well)) then
    !                write(modflow.iWEL,'(i8,2x,g15.5)') Modflow.GWF.nCells+i,modflow.CLN.PumpingRate(i),0
    !            end if
    !        end do
    !    end if
    !    
    !    if(allocated(modflow.SWF.PumpingRate)) then
    !        do i=1,modflow.SWF.nCells
    !            if(bcheck(Modflow.SWF.Cell_Is(i),Well)) then
    !                write(modflow.iWEL,'(i8,2x,2g15.5)') Modflow.GWF.nCells+modflow.CLN.nCells+i,modflow.SWF.PumpingRate(i),0
    !            end if
    !        end do
    !    end if
    !    
    !end subroutine WriteWELFile
    

    


    SUBROUTINE SGWF2BAS8OPEN(INUNIT,IOUT,IUNIT,CUNIT,&
                   NIUNIT,VERSION,INBAS,MAXUNIT, modflow)
!     ******************************************************************
!     OPEN FILES.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE NAMEFILEMODULE
      USE GWFBASMODULE, ONLY:IFLUSHS,CFLUSH
      INCLUDE 'openspec.inc'
      integer :: IUNIT(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*(*) VERSION
      CHARACTER*40 SPACES
      CHARACTER*300 LINE, FNAME
      CHARACTER*20 FILTYP
      LOGICAL LOP
      INTEGER, DIMENSION(99) ::TMPFLUSHS !kkz -tmp for list of binary output unit numbers to flush
      
      integer :: iout, inunit, maxunit, inbas, i, lenver, indent, niunit
      integer :: lloc, ityp1, ityp2
      real :: r
      integer :: n, istop, iu, istart, inam2, inam1, iflen, iflush, iopt2, iopt1, ii
      
      type (ModflowProject) Modflow

      
!     ---------------------------------------------------------------
!
!1------INITIALIZE CONSTANTS.
      INBAS=0
      NFILE=0
      IOUT=0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
5     CONTINUE
      SPACES=' '
      LENVER=LEN_TRIM(VERSION)
      INDENT=40-(LENVER+8)/2
      !ALLOCATE(CFLUSH) !kkz - allocate and initialize count of binary files to flush; move to alloc and dealloc if keeping
      CFLUSH=1         !kkz - initialize at 1 to account for mandatory output listing file
!
!2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUT.NE.0) WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
!
!3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      FILTYP=LINE(ITYP1:ITYP2)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) THEN
         IF(IOUT.EQ.0) THEN
            WRITE(*,11) FNAME(1:IFLEN),IU
   11       FORMAT(1X,/1X,'CANNOT OPEN ',A,' ON UNIT',I4,&
                   ' BECAUSE UNIT IS ALREADY BEING USED')
         ELSE
            WRITE(IOUT,11) FNAME(1:IFLEN),IU
         END IF
         CALL USTOP(' ')
      END IF
!
!4------KEEP TRACK OF LARGEST UNIT NUMBER
      IF (IU.GT.MAXUNIT) MAXUNIT = IU
!
!5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
!4A-IHM--ALLOW FOR UNFORMATTED PACKAGES WITH A NEGATIVE UNIT NUMBER
      IF(IU.LT.0)THEN
        FMTARG = 'BINARY'
        IU = ABS(IU)
      end if
!
!6------SPECIAL CHECK FOR 1ST FILE.
      IF(NFILE.EQ.0) THEN
        IF(FILTYP.EQ.'LIST') THEN
          !IOUT=IU
          Modflow.iLIST=IU
          file_open_flag(IU)=.true.
          Modflow.FNameLIST=FNAME(1:IFLEN)
          OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS='OLD',&
               FORM='FORMATTED',ACCESS='SEQUENTIAL',SHARE = 'DENYNONE'&
               , BUFFERED='NO')
!          WRITE(IOUT,60) MFVNAM,SPACES(1:INDENT),VERSION(1:LENVER)
!60        FORMAT(34X,'USG-TRANSPORT ',A,/,&
!                  6X,' FURTHER  DEVELOPMENTS BASED ON MODFLOW-USG',/,&
!                  A,'VERSION ',A,/)
!          WRITE(IOUT,78) FNAME(1:IFLEN),IOUT
!78        FORMAT(1X,'LIST FILE: ',A,/25X,'UNIT ',I4)
          TMPFLUSHS(CFLUSH)=IU   !kkz - store unit number of the listing file to flush in the tmp array
        ELSE
          WRITE(*,*)&
            ' FIRST ENTRY IN NAME FILE MUST BE "LIST".'
          CALL USTOP(' ')
        END IF
!7  Get next file name
        NFILE=1
        GO TO 10
      END IF
!
!8------CHECK FOR "BAS" FILE TYPE.
      IF(FILTYP.EQ.'BAS6') THEN
         INBAS=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
!
!9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(BINARY)' .OR.&
             FILTYP.EQ.'DATAGLO(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
         
!
!kkz check for both UNFORMATTED as well as BINARY per JCH
!9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(UNFORMATTED)' .OR.&
             FILTYP.EQ.'DATAGLO(UNFORMATTED)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
!
!10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.&
             LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
!
!11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              IUNIT(I)=IU
              FILSTAT='OLD    '
              FILACT=ACTION(1)
! --------------IHM - SYF FILES ARE FOR IHM
              IF(CUNIT(I) .EQ. 'SYF') THEN
                FILSTAT = 'UNKNOWN'
                FILACT= ACTION (1)
                SYIU = IU
                SYFNAME = FNAME
                SYIFLEN = IFLEN
                ACCARG='SEQUENTIAL'
              end if
! ---------IHM ----------------------------------
              GO TO 30
           END IF
20      CONTINUE
        WRITE(IOUT,21) LINE(ITYP1:ITYP2)
21      FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
30      CONTINUE
        END IF
!
!12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IARCVs(NFILE) = 1
      IFLUSH = 0
101   CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.&
           LINE(IOPT1:IOPT2).EQ.'OLD')&
           FILSTAT = LINE(IOPT1:IOPT2)
      end if
!12A -----IHM check for archive option for IHM
        IF(LINE(IOPT1:IOPT2).EQ.'FLUSH') THEN
          IFLUSH = 1
          WRITE(IOUT,70) FNAME(1:IFLEN)
   70     FORMAT(1X,'FLUSH OPTION IS USED FOR THIS FILE',A80)
        end if
!12A -----IHM check for archive option for IHM
        IF(LINE(IOPT1:IOPT2).EQ.'NO-ARCHIVE') THEN
          IARCVs(nfile) = 0
          WRITE(IOUT,71) FNAME(1:IFLEN)
   71     FORMAT(1X,'NO-ARCHIVE OPTION IN IHM IS USED:',&
          ' STRESS PERIOD INFORMATION WILL BE REPLACED FOR THIS FILE.',&
           A20)
        end if
! ----IHM ------------------------------------------------
      IF(LLOC.LT.300) GO TO 101
201   CONTINUE
!12A----Open file as read-only when 'OLD' is present to allow parallel
!12A----model runs to read data from file simultaneously.
      IF (FILACT.EQ.' ') THEN
        IF (FILSTAT.EQ.'OLD') THEN
          FILACT=ACTION(1)
        ELSE
          FILACT=ACTION(2)
        end if
      end if
!
!13-----WRITE THE FILE NAME AND OPEN IT.
      WRITE(IOUT,50) FNAME(1:IFLEN),&
          LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/&
       1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/&
       1X,'FORMAT:',A,3X,'ACCESS:',A)
!kkz - ALWAYS BUFFERING. NO IF STATEMENT OR OPTION
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG, SHARE = 'DENYNONE',&  !allows sharing of files for parallel PEST runs&
           ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
!      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG, SHARE = 'DENYNONE',  !allows sharing of files for parallel PEST runs&
!    1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,BUFFERED='YES',&
!    2      ERR=2000)
      IF(IFLUSH.NE.0) THEN   !kkz - if not STATUS=OLD, then assume an output file to flush at the end of each timestep
        CFLUSH = CFLUSH + 1    !kkz - increment counter for files to be flushed
        TMPFLUSHS(CFLUSH)=IU   !kkz - store unit number of an output file to flush in the tmp array
      end if
      
      !rgm workaround to assign binary output file unit numbers and names to Modflow data structure
      !IF(FILTYP.EQ.'DATA(BINARY)') THEN
         call LwrCse(FNAME(1:IFLEN))
         if(index(FNAME(1:IFLEN),'cln.hds') /= 0) then
             modflow.CLN.iHDS=IU
             file_open_flag(IU)=.true.
             modflow.CLN.FNameHDS=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.hds') /= 0) then
             modflow.SWF.iHDS=IU
             file_open_flag(IU)=.true.
             modflow.SWF.FNameHDS=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.hds') /= 0) then
             modflow.GWF.iHDS=IU
             file_open_flag(IU)=.true.
             modflow.GWF.FNameHDS=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'cln.ddn') /= 0) then
             modflow.CLN.iDDN=IU
             file_open_flag(IU)=.true.
             modflow.CLN.FNameDDN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.ddn') /= 0) then
             modflow.SWF.iDDN=IU
             file_open_flag(IU)=.true.
             modflow.SWF.FNameDDN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.ddn') /= 0) then
             modflow.GWF.iDDN=IU
             file_open_flag(IU)=.true.
             modflow.GWF.FNameDDN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'cln.cbb') /= 0) then
             modflow.CLN.iCBB=IU
             file_open_flag(IU)=.true.
             modflow.CLN.FNameCBB=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.cbb') /= 0) then
             modflow.SWF.iCBB=IU
             file_open_flag(IU)=.true.
             modflow.SWF.FNameCBB=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.cbb') /= 0) then
             modflow.GWF.iCBB=IU
             file_open_flag(IU)=.true.
             modflow.GWF.FNameCBB=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'.sms') /= 0) then
             modflow.FNameSMS=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'.oc') /= 0) then
             modflow.FNameOC=FNAME(1:IFLEN)
         end if
      !end if
     
      
      
!
!11a-------IHM save all file information for each unit that was opened.
      ii = nfile
      ius(ii) = iu
      fnames(ii) = fname(1:iflen)
      iflens(ii) = iflen
      filstats(ii) = filstat
      filacts(ii) = filact
      fmtargs(ii) = fmtarg
      accargs(ii) = accarg
      if(ius(ii).eq.iunit(12)) iarcvs(ii) = 1  ! archive OC file
      nfiles = nfile
! IHM--------------------------------------------------
      NFILE=NFILE+1
      GO TO 10
!
!14-----END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND BAS
!14-----FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,*) ' NAME FILE IS EMPTY.'
         CALL USTOP(' ')
      ELSE IF(INBAS.EQ.0) THEN
         WRITE(IOUT,*) ' BAS PACKAGE FILE HAS NOT BEEN OPENED.'
         CALL USTOP(' ')
      END IF
      CLOSE (UNIT=INUNIT)
!
      !kkz - allocate and fill IFLUSHS from tmp array
      ALLOCATE(IFLUSHS(CFLUSH))
      DO 1550 I=1,CFLUSH
          IFLUSHS(I) = TMPFLUSHS(I)
1550  CONTINUE
!
      RETURN
!
!15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,&
     7X,'SPECIFIED FILE STATUS: ',A,/&
     7X,'SPECIFIED FILE FORMAT: ',A,/&
     7X,'SPECIFIED FILE ACCESS: ',A,/&
     7X,'SPECIFIED FILE ACTION: ',A,/&
     2X,'-- STOP EXECUTION (SGWF2BAS7OPEN)')
      CALL USTOP(' ')
!
    END SUBROUTINE SGWF2BAS8OPEN

   
    
    subroutine ReadOC(Modflow)
        !SUBROUTINE SGWF2BAS7I(NLAY,INOC,IOUT,IFREFM,NIUNIT,ITRUNIT,ICUNIT)
        !     ******************************************************************
        !     SET UP OUTPUT CONTROL.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY: NSTP,ISSFLG,NPER,INCLN,IDPF,IUNIT,&
        NEQS
        USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,&
        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IBOUUN,LBBOSV,CBOUFM,&
        IAUXSV,IOFLG,VBVL,VBNM,ISPCFM,ISPCUN,CSPCFM,IATS,NPTIMES,&
        NPSTPS,DELTAT,TMINAT,TMAXAT,TADJAT,TCUTAT,&
        IDDREF,IDDREFNEW,IUDFAST,IUDFASTC,&
        IFAST,ISPFAST,ITSFAST,IUGFAST,IUCFAST,IFASTH,&
        IFASTC,ISPFASTC,ITSFASTC,IUGFASTC,IUCFASTC,IUMFASTC,&
        IUGBOOT,IUCBOOT,IUDBOOT,IBOOT,&
        BOOTSCALE,BOOTSLOPE,IBOOTSCALE,HREADBOOT
        USE CLN1MODULE, ONLY: ICLNHD, ICLNDD, ICLNIB,ICLNCN
        implicit none
    
        type (ModflowProject) Modflow
        
        CHARACTER*400 LINE
        integer :: inoc, lloc, in, istart, k
        real :: r
        integer :: istop, n, itrunit, icunit
        
        itrunit=0   !rgm for now assume no bct (block-centred transport) file opened
        icunit=Modflow.iCLN
        
        !     ------------------------------------------------------------------
        !
        !1-----ALLOCATE SPACE FOR IOFLG, VBVL, AND VBNM ARRAYS.
        ALLOCATE (IOFLG(NLAY,7))
        ALLOCATE (VBVL(4,NIUNIT))
        ALLOCATE (VBNM(NIUNIT))
        IDDREF=0
        IDDREFNEW=0
        !
        !1A------ASSIGN DEFAULT VALUES.
        CHEDFM=' '
        CDDNFM=' '
        CSPCFM=' '
        CBOUFM='(20I4)'
        IHEDFM=0
        IDDNFM=0
        ISPCFM=0
        IHEDUN=0
        IDDNUN=0
        ISPCUN=0
        IBOUUN=0
        IBDOPT=1
        LBHDSV=0
        LBDDSV=0
        LBBOSV=0
        IAUXSV=0
        !
        !2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
        !2------ACTIVE.
        IF( Modflow.iOC/=0) THEN
            INOC=Modflow.iOC
        else
            INOC=0
        end if
        IF(INOC.LE.0) THEN
            !
            !2A-----OUTPUT CONTROL IS INACTIVE. PRINT A MESSAGE LISTING DEFAULTS.
            WRITE(IOUT, 41)
            41    FORMAT(1X,/1X,'DEFAULT OUTPUT CONTROL',/1X,&
            'THE FOLLOWING OUTPUT COMES AT THE END OF EACH STRESS PERIOD:')
            WRITE(IOUT, 42)
            42    FORMAT(1X,'TOTAL VOLUMETRIC BUDGET')
            WRITE(IOUT, 43)
            43    FORMAT(1X,10X,'HEAD')
            !
            !2B-----SET DEFAULT FLAGS IN IOFLG SO THAT HEAD IS PRINTED FOR
            !2B-----EVERY LAYER.
            DO 80 K=1,NLAY
                IOFLG(K,1)=1
                IOFLG(K,2)=0
                IOFLG(K,3)=0
                IOFLG(K,4)=0
                IOFLG(K,5)=0
                IOFLG(K,6)=0
                IOFLG(K,7)=0
            80    CONTINUE
            GO TO 1000
        END IF
        !
        !3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
        !3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
        CALL URDCOM(INOC,IOUT,LINE)
        !--------------------------------------------------------------------------------
        !3A------CHECK FOR OPTIONS
        !ALLOCATE(IATS,NPTIMES,NPSTPS,IBUDFLAT,ICBCFLAT,IHDDFLAT,ISPCFLAT)
        !ALLOCATE(DELTAT,TMINAT,TMAXAT,TADJAT,TCUTAT)
        !ALLOCATE(IFAST,ISPFAST,ITSFAST,IUGFAST,IUCFAST,IFASTH)
        !ALLOCATE(IFASTC,ISPFASTC,ITSFASTC,IUGFASTC,IUCFASTC,IUMFASTC,&
        !IUDFAST,IUDFASTC)
        !ALLOCATE (IUGBOOT,IUCBOOT,IUDBOOT,IBOOT,IBOOTSCALE)
        !ALLOCATE (DTBOOTSCALE)
        IATS=0
        NPTIMES=0
        NPSTPS=0
        !-----initialize booting option flags
        IUGBOOT = 0
        IUCBOOT = 0
        IUDBOOT = 0
        IBOOT = 0
        IBOOTSCALE = 0
        !-----initialize fastforward option flags
        IFAST=0
        IFASTH=0
        IFASTC=0
        ISPFAST=0
        ITSFAST=0
        IUGFAST=0
        IUCFAST=0
        IUDFAST = 0
        !
        ISPFASTC=0
        ITSFASTC=0
        IUGFASTC=0
        IUCFASTC=0
        IUDFASTC = 0
        IUMFASTC = 0
        
        LLOC = 1
        10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'ATS'.OR.&
            LINE(ISTART:ISTOP).EQ.'ATSA') THEN
            !3B------READ KEYWORD OPTION ATS FOR ADAPTIVE TIME STEPPING.
            IATS = 1
        ELSEIF(LINE(ISTART:ISTOP).EQ.'NPTIMES') THEN
            !3C------IS KEWORD OPTION FOR NUMBER OF PRINT TIMES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPTIMES,R,IOUT,INOC)
        ELSEIF(LINE(ISTART:ISTOP).EQ.'NPSTPS') THEN
            !3C------IS KEWORD OPTION FOR NUMBER OF PRINT TIMES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPSTPS,R,IOUT,INOC)
            WRITE(IOUT,14) NPSTPS
            14      FORMAT(/1X,'OUTPUT PROVIDED EVERY', I8,' TIME STEPS (NPSTPS)')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'FASTFORWARD') THEN
            IFAST = 1
            IFASTH = 1
            !3C------IS KEWORD OPTION FOR FASTFORWARD FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPFAST,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSFAST,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGFAST,R,IOUT,INOC)
            WRITE(IOUT,15) ISPFAST,ITSFAST,IUGFAST
            15      FORMAT(/1X,'FASTFORWARDING TO STRESS PERIOD,',I10,&
            ' AND TIME-STEP',I10&
            /10X,'READING FASTFORWARD GWF HEADS FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCFAST,R,IOUT,INOC)
                WRITE(IOUT,16) IUCFAST
                16        FORMAT(10X,'READING FASTFORWARD CLN HEADS FROM UNIT',I5)
            end if
            IF(IDPF.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDFAST,R,IOUT,INOC)
                WRITE(IOUT,17) IUDFAST
                17        FORMAT(10X,'READING FASTFORWARD DDF HEADS FROM UNIT',I5)
            end if
        ELSEIF(LINE(ISTART:ISTOP).EQ.'FASTFORWARDC') THEN
            IFAST = 1
            IFASTC = 1
            !3C------IS KEWORD OPTION FOR FASTFORWARD OF CONC FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPFASTC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSFASTC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGFASTC,R,IOUT,INOC)
            WRITE(IOUT,25) ISPFASTC,ITSFASTC,IUGFASTC
            25      FORMAT(/1X,'FASTFORWARDING TO STRESS PERIOD,',I10,&
            ' AND TIME-STEP',I10&
            /10X,'READING FASTFORWARD GWF CONC FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCFASTC,R,IOUT,INOC)
                WRITE(IOUT,26) IUCFASTC
                26        FORMAT(10X,'READING FASTFORWARD CLN CONC FROM UNIT',I5)
            end if
            IF(IUNIT(30).GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDFASTC,R,IOUT,INOC)
                WRITE(IOUT,27) IUDFASTC
                27        FORMAT(10X,'READING FASTFORWARD DDT CONCS FROM UNIT',I5)
            end if
            !        IF(ImdT.GT.0)THEN
            !          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUMFASTC,R,IOUT,INOC)
            !          WRITE(IOUT,28) IUMFASTC
            !28        FORMAT(10X,'READING FASTFORWARD MATRIX CONCS FROM UNIT',I5)
            !        end if
        ELSEIF(LINE(ISTART:ISTOP).EQ.'BOOTSTRAPPING') THEN
            !3C------IS KEWORD OPTION FOR BOOTSTRAPPING THE NEW ESTIMATE FOR HEAD AT FIRST ITERATION FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGBOOT,R,IOUT,INOC)
            WRITE(IOUT,29) IUGBOOT
            29      FORMAT(/1X,'BOOTSTRAPPING IS DONE FOR TRANSIENT SIMULATION,',/&
            10X,'READING BOOTSTRAP GWF HEADS FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCBOOT,R,IOUT,INOC)
                WRITE(IOUT,30) IUCBOOT
                30        FORMAT(10X,'READING BOOTSTRAP CLN HEADS FROM UNIT',I5)
            end if
            IF(IUNIT(30).GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDBOOT,R,IOUT,INOC)
                WRITE(IOUT,31) IUDBOOT
                31        FORMAT(10X,'READING BOOTSTRAP DDF HEADS FROM UNIT',I5)
            end if
        END IF
        !
        IF(LLOC.LT.200) GO TO 10
        !
        !-------ALLOCATE SPACE FOR BOOTSTRAPING ARRAYS
        IF(IUGBOOT.NE.0)THEN
            ALLOCATE (BOOTSCALE(NEQS),BOOTSLOPE(NEQS),HREADBOOT(NEQS))
            BOOTSCALE = 1.0
        end if
        !-------SET DEFAULTS FOR ADAPTIVE TIME STEPPING
        IF(NPSTPS.GT.0.OR.NPTIMES.GT.0) IATS=1
        IF(NPSTPS.EQ.0) NPSTPS=100000000
        IF(IATS.EQ.1)THEN
            WRITE(IOUT,13)
            13   FORMAT(/1X,'ADAPTIVE TIME STEPPING PERFORMED. ATS',&
            ' VARIABLES WILL BE READ.'/1X,61('-'))
            DO K=1,NPER
                IF(ISSFLG(K).EQ.1.AND.ITRUNIT.EQ.0) THEN
                    NSTP(K) = 1
                ELSE
                    NSTP(K) = 1000000
                end if
            end do
            DELTAT = 1.0
            TMINAT = 1.0E-10
            TMAXAT = 1.0E10
            TADJAT = 2.0
            TCUTAT = 5.0
        end if
        !------------------------------------------------------------------------------
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
        !
        !4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
        !4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT" OR CONC OR CONCENTRATION.
        IF(LINE(ISTART:ISTOP).NE.'PERIOD' .AND. LINE(ISTART:ISTOP).NE.&
        'HEAD' .AND. LINE(ISTART:ISTOP).NE.'DRAWDOWN' .AND.&
        LINE(ISTART:ISTOP).NE.'COMPACT' .AND.&
        LINE(ISTART:ISTOP).NE.'IBOUND'.AND.&
        LINE(ISTART:ISTOP).NE.'CONC'.AND.&
        LINE(ISTART:ISTOP).NE.'CONCENTRATION'.AND.&
        LINE(ISTART:ISTOP).NE.'FASTFORWARD'.AND.&
        LINE(ISTART:ISTOP).NE.'FASTFORWARDC'.AND.&
        LINE(ISTART:ISTOP).NE.'BOOTSTRAPPING'.AND.&
        LINE(ISTART:ISTOP).NE.'ATSA') THEN
            !4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
            WRITE(IOUT,102)
            102    FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')
            IF(ITRUNIT.EQ.0)THEN
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(4I10)') IHEDFM,IDDNFM,IHEDUN,IDDNUN
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
                END IF
                WRITE(IOUT,103) IHEDFM,IDDNFM
                103     FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                '    DRAWDOWN PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,104) IHEDUN,IDDNUN
                104      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
            ELSE
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(6I10)')IHEDFM,IDDNFM,IHEDUN,IDDNUN,ISPCFM,ISPCUN
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCUN,R,IOUT,INOC)
                END IF
                WRITE(IOUT,113) IHEDFM,IDDNFM,ISPCFM
                113     FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                '    DRAWDOWN PRINT FORMAT CODE IS',I4,&
                '        CONC PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,114) IHEDUN,IDDNUN,ISPCUN
                114      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,&
                '         CONC WILL BE SAVED ON UNIT ',I4)
            end if
            IPEROC=-1
            ITSOC=-1
            !4B---READ OUTPUT TIME SERIES VECTOR
            IF(NPTIMES.GT.0) CALL PTIMES1RP(INOC,IOUT,modflow)
        ELSE
            !----------------------------------------------------------------------------------------
            !4C---FOR ALPHABETIC OC, READ OUTPUT TIME SERIES VECTOR FIRST
            IF(NPTIMES.GT.0) CALL PTIMES1RP(INOC,IOUT,modflow)
            !4D-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
            CALL SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
        end if
        IF(ICUNIT.GT.0) THEN
            IF(ICLNHD.LT.0) ICLNHD = IHEDUN
            IF(ICLNDD.LT.0) ICLNDD = IDDNUN
            IF(ICLNIB.LT.0) ICLNIB = IBOUUN
            IF(ICLNCN.LT.0) ICLNCN = ISPCUN
        end if
        !
        !5------RETURN.
        1000 RETURN
    END subroutine ReadOC
    
    SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
        !     ******************************************************************
        !     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY: ITRNSP
        USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,&
                    CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IATS,IDDREFNEW,&
                    IBOUUN,LBBOSV,CBOUFM,IAUXSV,ISPCFM,ISPCUN,CSPCFM
        !
        CHARACTER*400 LINE
        
        integer :: inoc, iout, lloc, istart, istop
        real :: r
        integer :: n
        !     ------------------------------------------------------------------
        !
        !1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
        !1------FOR IPEROC AND ITSOC.
        WRITE(IOUT,91)
        91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',&
        ' FOR WHICH OUTPUT IS DESIRED')
        IPEROC=9999
        ITSOC=9999
        !
        !2------LOOK FOR ALPHABETIC WORDS:
        
        !2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
        !2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
        !2A-----STEP NUMBER FOR LATER USE.
    100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            !2Ai-------SKIP TIME STEP IF ADAPTIVE TIME STEPPING
            IF(IATS.NE.0) GO TO 20
            IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            20       CONTINUE
            IF(ITRNSP.EQ.0)THEN
                WRITE(IOUT,101) IHEDFM,IDDNFM
                101      FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                    '    DRAWDOWN PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,102) IHEDUN,IDDNUN
                102      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                    '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
            ELSE
                WRITE(IOUT,113) IHEDFM,IDDNFM,ISPCFM
                113      FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                    '    DRAWDOWN PRINT FORMAT CODE IS',I4,&
                    '        CONC PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,122) IHEDUN,IDDNUN,ISPCUN
                122      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                    '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,&
                    '        CONCS WILL BE SAVED ON UNIT ',I4)
            end if
            !2Aii------READ DDREFERENCE FLAG
            IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
                IDDREFNEW=1
            ELSE
                IDDREFNEW=0
            END IF
            !
            GO TO 1000
            
        !
        !2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
        !2B-----FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,&
                        INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CHEDFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,103) CHEDFM
                103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBHDSV=1
                        WRITE(IOUT,104)
                104     FORMAT(1X,'SAVED HEADS WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
            
        !
        !2C-----LOOK FOR "DRAWDOWN PRINT ..." AND "DRAWDOWN SAVE ...".
        !2C-----IF FOUND, SET APPROPRIATE FLAGS
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,&
                            INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CDDNFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,105) CDDNFM
                105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBDDSV=1
                        WRITE(IOUT,106)
                106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
            
        !
        !2B-----LOOK FOR "CONC PRINT ..." AND "CONC SAVE ...".  IF
        !2B-----FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'CONC'.OR.LINE(ISTART:ISTOP)&
            .EQ.'CONCENTRATION') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCUN,R,IOUT,&
                        INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CSPCFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,115) CSPCFM
                115          FORMAT(1X,'CONCS WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBHDSV=1
                        WRITE(IOUT,116)
                116     FORMAT(1X,'SAVED CONCS WILL BE LABELED')
                    END IF
                ELSE
                        GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
        !
        !2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
        !2D-----IF FOUND, SET APPROPRIATE FLAG.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
                IBDOPT=2
                WRITE(IOUT,107)
                107       FORMAT(1X,&
                'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
                    LINE(ISTART:ISTOP).EQ.'AUX') THEN
                    IAUXSV=1
                    WRITE(IOUT,108)
                108 FORMAT(1X,&
                    'AUXILIARY DATA WILL BE SAVED IN CELL-BY-CELL BUDGET FILES')
                END IF
            ELSE
                GO TO 2000
            END IF
 
        !
        !2E-----LOOK FOR  "IBOUND SAVE ...".  IF FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,&
                        INOC)
                    WRITE(IOUT,111) IBOUUN
                111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CBOUFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,112) CBOUFM
                112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBBOSV=1
                        WRITE(IOUT,109)
                109             FORMAT(1X,'SAVED IBOUND WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
        
        !2F-------FOR ADAPTIVE TIME STEPPING WITH ALPHABTIC INPUT READ NEXT RECORD
        ELSE IF(LINE(ISTART:ISTOP).EQ.'ATSA') THEN
            GO TO 110
        
        ELSE IF(LINE(ISTART:ISTOP).EQ.'FASTFORWARD') THEN
            GO TO 110
        
        ELSE IF(LINE(ISTART:ISTOP).EQ.'FASTFORWARDC') THEN
            GO TO 110
        !
        !2F-----ERROR IF UNRECOGNIZED WORD.
        ELSE
            GO TO 2000
        END IF
        !
        !3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
        !3------LINES.  GO BACK AND DECODE IT.
        110 READ(INOC,'(A)',END=1000) LINE
        IF(LINE.EQ.' ') GO TO 110
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
        GO TO 100
        !
        !4------RETURN.
        1000 RETURN
        !
        !5------ERROR DECODING INPUT DATA.
        2000 WRITE(IOUT,2001) LINE
        2001 FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
        CALL USTOP(' ')
    END SUBROUTINE SGWF2BAS7J
    
    SUBROUTINE PTIMES1RP(INOC,IOUT,modflow)
        !       ******************************************************************
        !        READ PRINT TIME ARRAY.
        !       ******************************************************************
        !
        !       SPECIFICATIONS:
        !       ------------------------------------------------------------------
                USE GWFBASMODULE, ONLY: TIMOT,ITIMOT,TIMOTC,ITIMOTC,&
                    TMINAT,NPTIMES
        type (ModflowProject) Modflow
                
        !
        
                integer :: inoc, iout, it, i
        !     ------------------------------------------------------------------
        !
        !1------READ PRINT TIME ARRAY
              ALLOCATE(TIMOT(NPTIMES+1),TIMOTC(NPTIMES+1))
              ALLOCATE(modflow.TIMOT(NPTIMES+1))
              
              !ALLOCATE(ITIMOT,ITIMOTC)
              ITIMOT=1
              ITIMOTC = 1   ! USE SEPARATE INDEX AND ARRAY FOR CONCENTRATIONS
        !
              WRITE(IOUT,11)NPTIMES
11            FORMAT(/10X,  'OUTPUT WILL BE PRINTED AT FOLLOWING',I8,' TIMES'/&
                     10X,  50('-'))
              
              modflow.ntime=nptimes
        !
              READ(INOC,*) (TIMOT(IT),IT=1,NPTIMES)
              WRITE(IOUT,'(10(1PG15.7))') (TIMOT(IT),IT=1,NPTIMES)
              TIMOT(NPTIMES+1) = 1.0E20
        !2------PERFORM CONSISTENCY CHECKS
              DO IT=1,NPTIMES-1
                IF(TIMOT(IT+1)-TIMOT(IT).LE.TMINAT) THEN
                  WRITE(IOUT,12)
        12        FORMAT( 'PRINT TIMES ARE NOT SEQUENTIALLY INCREASING',&
                 ' OR ARE LESS THAN TMINAT APART')
                  STOP
                end if
              end do
        !3 ---FILL TIMOTC WITH TIMOT FOR TRANSPORT
              DO I = 1,NPTIMES+1
                TIMOTC(I) = TIMOT(I)
                modflow.TIMOT(I) = TIMOT(I)
              end do
        !
        !4------RETURN.
         1000 RETURN
    END SUBROUTINE PTIMES1RP
    
    SUBROUTINE ReadLPF(Modflow)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL, ONLY:NLAY,ITRSS,LAYHDT,LAYHDS,&
                         NCNFBD,IOUT,NOVFC,itrnsp,&
                         NODES,IUNSTR,ICONCV,NOCVCO,NJAS,IWADI,&
                         IDEALLOC_LPF,ARAD,iunsat,&
                         IOUTNORMAL
              USE GWFBCFMODULE,ONLY:IBCFCB,IWDFLG,IWETIT,IHDWET,WETFCT,HDRY,CV,&
                              LAYCON,LAYAVG,SC1,SC2,WETDRY,CHANI,IHANISO,&
                              IKCFLAG,LAYWET,ISFAC,ITHFLG,LAYAVGV,&
                              LAYTYP,LAYVKA,LAYSTRT,alpha,beta,sr,brook,&
                              LAYFLG,VKA,VKCB,HANI,HK,IBPN,BP,IDRY,IALTSTO,&
                ITABRICH,INTRICH,IUZONTAB,RETCRVS,NUTABROWS,NUZONES
              
              implicit none
              type (ModflowProject) Modflow

        !
              CHARACTER*14 LAYPRN(5),AVGNAM(5),TYPNAM(3),VKANAM(2),WETNAM(2),&
                         HANNAM
              DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','   LOG+ARITHM ',&
                '   ARITHMETIC ','FINITE ELEMENT'/
              DATA TYPNAM/'     CONFINED ','  CONVERTIBLE ','     UPSTREAM '/
              DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
              DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
              DATA HANNAM/'      VARIABLE'/
              CHARACTER*400 LINE
              CHARACTER*24 ANAME(10),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /'UNSAT PARAMETER ZONE MAP'/
              
              integer :: in, lloc, istop, istart, i, k
              real :: r
              integer :: nplpf, nopchk, n
              integer :: izon, itrows,inlak
              integer :: NCNVRT
                integer :: NHANI
                integer :: NWETD
                integer :: ILAYUNSAT
                integer :: NPHK
                integer :: NPVKCB
                integer :: NPVK
                integer :: NPVANI
                integer :: NPSS
                integer :: NPSY
                integer :: NPHANI

              
              in=modflow.iLPF
              rewind(modflow.iLPF)
              iout=FNumEco
              inlak=modflow.iLAK
        !
        !     ------------------------------------------------------------------
        !1------Allocate scalar data.
              !ALLOCATE(ISFAC,ITHFLG)
              ZERO=0.
        !
        !2------IDENTIFY PACKAGE
              WRITE(IOUT,1) IN
            1 FORMAT(1X,/1X,'LPF -- LAYER-PROPERTY FLOW PACKAGE, VERSION 7',&
             ', 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
        !
        !3------READ COMMENTS AND ITEM 1.
        !csp commented out to be backward compatible with LPF package of MF2K5.
        !csp      IF(IFREFM.EQ.0) THEN
        !csp        IF(IUNSTR.NE.0)THEN
        !csp          READ(IN,2)IBCFCB,HDRY,NPLPF,IKCFLAG
        !csp        ELSE
        !csp          READ(IN,2)IBCFCB,HDRY,NPLPF
        !csp        end if
        !csp      ELSE

                CALL URDCOM(IN,IOUT,LINE)
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBCFCB,R,IOUT,IN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLPF,R,IOUT,IN)
                IF(IUNSTR.NE.0)&
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IKCFLAG,R,IOUT,IN)
        !csp      end if
        2     FORMAT(I10,F10.3,2I10)
        !
        !3A-----WRITE ITEM 1
              IF(IBCFCB.LT.0) WRITE(IOUT,8)
            8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',&
               ' WHEN ICBCFL IS NOT 0')
              IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB
            9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
              WRITE(IOUT,11) HDRY
           11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
              IF(NPLPF.GT.0) THEN
                 WRITE(IOUT,15) NPLPF
           15    FORMAT(1X,I5,' Named Parameters     ')
              ELSE
                 NPLPF=0
                 WRITE(IOUT,'(A)') ' No named parameters'
              END IF
        !
              IF(IUNSTR.NE.0)THEN
                IF(IKCFLAG.EQ.0)WRITE(IOUT,39)
           39   FORMAT(1X,'IKCFLAG=0, NODAL INPUT OF HY AND CV')
                IF(IKCFLAG.EQ.1)WRITE(IOUT,41)
           41   FORMAT(1X,'IKCFLAG=1, CONNECTIVITY INPUT OF HY',1X,&
                     '(OR TRAN FOR CONFINED) AND CV')
                IF(IKCFLAG.EQ.-1)WRITE(IOUT,44)
           44   FORMAT(1X,'IKCFLAG=-1, CONNECTIVITY INPUT OF CONDUCTANCE')
              end if
        !
        !3B-----GET OPTIONS.
              ISFAC=0
              ICONCV=0
              ITHFLG=0
              NOCVCO=0
              NOPCHK=0
              STOTXT=ANAME(6)
           20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
              IF(LINE(ISTART:ISTOP).EQ.'STORAGECOEFFICIENT') THEN
                 ISFAC=1
                 STOTXT=ANAME(9)
                 WRITE(IOUT,21)
           21    FORMAT(1X,'STORAGECOEFFICIENT OPTION:',/,&
                  1X,'Read storage coefficient rather than specific storage')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'CONSTANTCV') THEN
                 ICONCV=1
                 WRITE(IOUT,23)
           23    FORMAT(1X,'CONSTANTCV OPTION:',/,1X,'Constant vertical',&
                      ' conductance for convertible layers')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'THICKSTRT') THEN
                 ITHFLG=1
                 WRITE(IOUT,25)
           25    FORMAT(1X,'THICKSTRT OPTION:',/,1X,'Negative LAYTYP indicates',&
              ' confined layer with thickness computed from STRT-BOT')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOCVCORRECTION') THEN
                 NOCVCO=1
                 WRITE(IOUT,27)
           27    FORMAT(1X,'NOCVCORRECTION OPTION:',/,1X,&
                 'Do not adjust vertical conductance when applying',&
                           ' the vertical flow correction')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOVFC') THEN
                 NOVFC=1
                 IWADI = 0
                 NOCVCO=1
                 WRITE(IOUT,29)
           29    FORMAT(1X,'NOVFC OPTION:',/,1X,&
                 'Do not apply the vertical flow correction')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPARCHECK') THEN
                 NOPCHK=1
                 WRITE(IOUT,30)
           30    FORMAT(1X,'NOPARCHECK  OPTION:',/,1X,&
                 'For data defined by parameters, do not check to see if ',&
                     'parameters define data at all cells')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'BUBBLEPT') THEN
                 IBPN=1
                 WRITE(IOUT,31)
           31    FORMAT(1X,'BUBBLEPT  OPTION:',/,1X,&
                 'For Richards equation the bubble point head is also ',&
                     'input at all cells')
                 allocate(bp(nodes))
                 DO N=1,NODES
                   BP(NODES) = 0.0
                 end do
              ELSE IF(LINE(ISTART:ISTOP).EQ.'FULLYDRY') THEN
                 IDRY=1
                 WRITE(IOUT,32)
           32    FORMAT(1X,'FULLYDRY  OPTION:',/,1X,&
                 'For Richards equation, residual saturation is only applied ',&
                 'to relative permeability and soil saturation can vary from ',&
                 '0 to 1')
               ELSE IF(LINE(ISTART:ISTOP).EQ.'TABRICH') THEN
                 ITABRICH=1
                 !ALLOCATE(NUZONES,NUTABROWS)
                 WRITE(IOUT,33)
           33    FORMAT(1X,'TABULAR INPUT OPTION:',/,1X,&
                 'For Richards equation the retention ',&
                 'and relative permeability curves are provided ',&
                 'as tabular input for different soil types')
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUZONES,R,IOUT,IN)
                 WRITE(IOUT,36) NUZONES
        36       FORMAT(1X,'NUMBER OF SOIL TYPE ZONES = ', I10)
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUTABROWS,R,IOUT,IN)
                 WRITE(IOUT,37) NUTABROWS
        37       FORMAT(1X,'NUMBER OF ROWS OF TABULAR INPUT = ', I10)
              ELSE IF(LINE(ISTART:ISTOP).EQ.'INTRICH') THEN
                 INTRICH=1
                 WRITE(IOUT,34)
           34    FORMAT(1X,'INTEGRATED CURVE OPTION:',/,1X,&
                 'For Richards equation with tabular input, the retention ',&
                 'and relative permeability curves are integrated over the ',&
                 'grid block thickness')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'ALTSTO') THEN
                 IALTSTO=1
                 WRITE(IOUT,35)
           35    FORMAT(1X,'ALTERNATIVE STORAGE FORMULATION (IALTSTO = 1)')
              END IF
        !
              IF(LLOC.LT.200) GO TO 20
        !
        !-------READ STUFF TABULAR INPUT OF RICHARDS EQUATION
              IF(ITABRICH.NE.0) THEN
        !-------DIMENSION AND READ ZONE MAP
                ALLOCATE(IUZONTAB(NODES))
                CALL U1DINT(IUZONTAB,ANAME(10),NODES,0,IN,IOUT)
        !-------DIMENSION AND READ TABLES
                ALLOCATE(RETCRVS(3,NUTABROWS,NUZONES))
        !
                DO IZON = 1,NUZONES
                  WRITE(IOUT,65) IZON
        65        FORMAT(/10X,'RETENTION TABLE NUMBER',I5/10X,27('-')/&
               10X,'CAPILLARY HEAD',7X,'SATURATION',3X,'RELATIVE PERMEABILITY')
                  DO ITROWS = 1,NUTABROWS
                    READ (IN,*) (RETCRVS(I,ITROWS,IZON),I=1,3)
                    WRITE(IOUT,66)(RETCRVS(I,ITROWS,IZON),I=1,3)
        66          FORMAT(10X,E14.6,3X,E14.6,7X,E14.6)
                  end do
                end do
              end if
        !
        !4------ALLOCATE AND READ LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
              ALLOCATE(LAYTYP(NLAY))
              ALLOCATE(LAYAVG(NLAY))
              ALLOCATE(CHANI(NLAY))
              ALLOCATE(LAYVKA(NLAY))
              ALLOCATE(LAYSTRT(NLAY))
              ALLOCATE(LAYWET(NLAY))
              ALLOCATE(LAYCON(NLAY))
              ALLOCATE(LAYHDT(NLAY))
              ALLOCATE(LAYHDS(NLAY))
              ALLOCATE(LAYAVGV(NLAY))
              READ(IN,*) (LAYTYP(K),K=1,NLAY)
              READ(IN,*) (LAYAVG(K),K=1,NLAY)
              READ(IN,*) (CHANI(K),K=1,NLAY)
              READ(IN,*) (LAYVKA(K),K=1,NLAY)
              READ(IN,*) (LAYWET(K),K=1,NLAY)
              DO K=1,NLAY
                  LAYAVGV(K) = 0  ! FINITE DIFFERENCE HARMONIC AVERAGING IN VERTICAL DIRECTION DEFAULT
              end do

        !
        !4A-----PRINT A TABLE OF VALUES FOR LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET.
              WRITE(IOUT,47)
           47 FORMAT(1X,/3X,'LAYER FLAGS:',/1X,&
              'LAYER       LAYTYP        LAYAVG         CHANI ',&
              '       LAYVKA        LAYWET',/1X,75('-'))
              DO 50 K=1,NLAY
              WRITE(IOUT,48) K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K)
           48 FORMAT(1X,I4,2I14,1PE14.3,2I14)
        !-----------------------------------------------------------------------------------
        !4A2 ---ALSO CONVERT LAVAVGV TO 1 (WEIGHTED ARITH AVERAGING FOR LPF)
        !4A2----WHEN LAYAVG IS 4 FOR FINITE ELEMENT AVERAGING
              IF(LAYAVG(K).EQ.4)THEN
                LAYAVGV(K) = 1
              end if
        !
        !4B-----SET OPTIONS FOR BCF
              IF(LAYTYP(K).EQ.0)THEN
                LAYCON(K) = 0
              ELSEIF(LAYTYP(K).GT.0.AND.LAYTYP(K).LT.4)THEN
                LAYCON(K) = 3
              ELSEIF(LAYTYP(K).EQ.4)THEN
                LAYCON(K) = 4
              ELSEIF(LAYTYP(K).EQ.5)THEN
                LAYCON(K) = 5
              ELSEIF(LAYTYP(K).LT.0)THEN
                IF(ITHFLG.EQ.1)THEN
                  LAYCON(K) = 0
                ELSE
                  LAYCON(K) = 3
                end if
              end if
              IDEALLOC_LPF = 1
              IF(INLAK.GT.0) IDEALLOC_LPF = 2  !DEALLOCATE ONLY AFTER LAK7U1RP IS DONE
              IF(ICONCV.EQ.0) IDEALLOC_LPF = 0 ! NEED LPF ARRAYS FOR VARIABLE CV OPTION
              IF(CHANI(1).LE.0) IDEALLOC_LPF = 0 ! NEED LPF ARRAYS FOR VARIABLE ANISOTROPY
        !
        !4C-----SET GLOBAL HEAD-DEPENDENT TRANSMISSIVITY AND STORAGE FLAGS.
              IF (LAYTYP(K).NE.0) THEN
                LAYHDT(K)=1
                LAYHDS(K)=1
              ELSE
                LAYHDT(K)=0
                LAYHDS(K)=0
              end if
           50 CONTINUE

        !
        !4D-----SET LAYSTRT AND RESET LAYTYP IF THICKSTRT OPTION IS ACTIVE.
              DO 60 K=1,NLAY
              LAYSTRT(K)=0
              IF(LAYTYP(K).LT.0 .AND. ITHFLG.NE.0) THEN
                 LAYSTRT(K)=1
                 LAYTYP(K)=0
                 LAYHDT(K)=0
                 LAYHDS(K)=0
                 WRITE(IOUT,57) K
           57    FORMAT(1X,'Layer',I5,&
               ' is confined because LAYTYP<0 and THICKSTRT option is active')
              END IF
           60 CONTINUE
        !
        !4E-----SET HORIZONTAL ANISOTROPY FLAG
              IHANISO = 0
              DO K=1,NLAY
                IF(ABS(CHANI(K) - 1.0).GT.1.0E-6) IHANISO = 1
              end do
              IF(IHANISO.EQ.1) THEN
                !ALLOCATE(IOUTNORMAL)
                IOUTNORMAL = 0
                ALLOCATE(ARAD(NJAS))
              end if
        !
        !4F-----BASED ON LAYTYP, LAYAVG, CHANI, LAYWET, COUNT THE NUMBER OF EACH
        !4F-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
        !4F-----POINTERS IN LAYTYP, CHANI, AND LAYWET FOR CONVENIENT ACCESS
        !4F-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
              NCNVRT=0
              NHANI=0
              NWETD=0
              WRITE(IOUT,67)
           67 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,&
               '                       INTERBLOCK     HORIZONTAL',&
               '    DATA IN',/1X,&
               '        LAYER TYPE   TRANSMISSIVITY   ANISOTROPY',&
               '   ARRAY VKA   WETTABILITY',/1X,&
               'LAYER      (LAYTYP)      (LAYAVG)       (CHANI)',&
               '      (LAYVKA)      (LAYWET)',/1X,75('-'))
              ILAYUNSAT = 0
              DO 100 K=1,NLAY
        !
              IF(LAYTYP(K).EQ.5) ILAYUNSAT = 1 ! AT LEAST 1 LAYER USES RICHARDS EQUATION
        !
              IF(LAYTYP(K).NE.0) THEN
                 NCNVRT=NCNVRT+1
                 LAYTYP(K)=NCNVRT
              END IF
              IF(CHANI(K).LE.ZERO) THEN
                 NHANI=NHANI+1
                 CHANI(K)=-NHANI
              END IF
              IF(LAYWET(K).NE.0) THEN
                 IF(LAYTYP(K).EQ.0) THEN
                    WRITE(IOUT,*)&
                       ' LAYWET is not 0 and LAYTYP is 0 for layer:',K
                    WRITE(IOUT,*) ' LAYWET must be 0 if LAYTYP is 0'
                    CALL USTOP(' ')
                 ELSE
                    NWETD=NWETD+1
                    LAYWET(K)=NWETD
                 END IF
              END IF
              IF(LAYAVG(K).LT.0 .OR. LAYAVG(K).GT.4) THEN
                 WRITE(IOUT,74) LAYAVG(K)
           74    FORMAT(1X,I8,&
                 ' IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, 2, 3 or 4')
                 CALL USTOP(' ')
              END IF
              LAYPRN(1)=TYPNAM(1)
              IF(LAYTYP(K).NE.0) LAYPRN(1)=TYPNAM(2)
              IF(LAYCON(K).EQ.4.OR.LAYCON(K).EQ.5) LAYPRN(1)=TYPNAM(3)
              LAYPRN(2)=AVGNAM(LAYAVG(K)+1)
              IF(LAYAVGV(K).NE.0) LAYPRN(2) = AVGNAM(5)
              IF(CHANI(K).LE.0) THEN
                 LAYPRN(3)=HANNAM
              ELSE
                 WRITE(LAYPRN(3),'(1PE14.3)') CHANI(K)
              END IF
              LAYPRN(4)=VKANAM(1)
              IF(LAYVKA(K).NE.0) LAYPRN(4)=VKANAM(2)
              LAYPRN(5)=WETNAM(1)
              IF(LAYWET(K).NE.0) LAYPRN(5)=WETNAM(2)
              WRITE(IOUT,78) K,(LAYPRN(I),I=1,5)
           78 FORMAT(1X,I4,5A)
          100 CONTINUE
        !
        !4G-----PRINT WETTING INFORMATION.
              IF(NWETD.EQ.0) THEN
                 WRITE(IOUT,13)
           13    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
                 IWDFLG=0
              ELSE
                 WRITE(IOUT,12) NWETD
           12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
                 IWDFLG=1
                 READ(IN,*) WETFCT,IWETIT,IHDWET
                 IF(IWETIT.LE.0) IWETIT=1
                 WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
                 WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
                 WRITE(IOUT,*) ' IHDWET=',IHDWET
              END IF
        !
        !5------ALLOCATE MEMORY FOR ARRAYS.
              ALLOCATE(CV(NODES))
        !
              ALLOCATE(LAYFLG(6,NLAY))
              ALLOCATE(HK(NODES))
              ALLOCATE(modflow.GWF.Kh(NODES))
              ALLOCATE(VKA(NODES))
              IF(NCNFBD.GT.0) THEN
                 ALLOCATE(VKCB(NODES))
              ELSE
                 ALLOCATE(VKCB(1))
              END IF
              IF(ITRSS.NE.0) THEN
                 ALLOCATE(SC1(NODES))
              ELSE
                 ALLOCATE(SC1(1))
              END IF
              IF(ITRSS.NE.0 .AND. NCNVRT.GT.0 .or. itrnsp.ne.0) THEN
                 ALLOCATE(SC2(NODES))
                 SC2 = 0.0
              ELSE
                 ALLOCATE(SC2(1))
              END IF
              IF(NHANI.GT.0) THEN
                 ALLOCATE(HANI(NODES))
              ELSE
                 ALLOCATE(HANI(1))
              END IF
              IF(NWETD.GT.0) THEN
                 ALLOCATE(WETDRY(NODES))
              ELSE
                 ALLOCATE(WETDRY(1))
              END IF
        !
              if(iunsat.eq.1.OR.ILAYUNSAT.EQ.1)then
                if(itabrich.eq.0)then
                  ALLOCATE(alpha(NODES),beta(nodes),sr(nodes),brook(nodes))
                  DO N=1,NODES
                    ALPHA(N) =0.0
                    BETA(N) = 1.0
                    SR(N) = 0.999
                    BROOK(N) = 1.0
                  end do
                end if
                IF(IUNSAT.EQ.1)THEN
                  DO K=1,NLAY
                    IF(LAYCON(K).NE.0) LAYCON(K)=5
                  end do
                end if
              end if
        !
        !6------READ PARAMETER DEFINITIONS
              NPHK=0
              NPVKCB=0
              NPVK=0
              NPVANI=0
              NPSS=0
              NPSY=0
              NPHANI=0
              IF(NPLPF.GT.0) THEN
                 WRITE(IOUT,115)
          115    FORMAT(/,' PARAMETERS DEFINED IN THE LPF PACKAGE')
                 DO 120 K=1,NPLPF
                 CALL UPARARRRP(IN,IOUT,N,1,PTYP,1,0,-1)
        !   Note that NPHK and the other NP variables in
        !   this group are used only as flags, not counts
                 IF(PTYP.EQ.'HK') THEN
                    NPHK=1
                 ELSE IF(PTYP.EQ.'HANI') THEN
        !6A-----WHEN A HANI PARAMETER IS USED, THEN ALL HORIZONTAL ANISOTROPY
        !6A-----MUST BE DEFINED USING PARAMETERS.  ENSURE THAT ALL CHANI <= 0
                    DO 118 I = 1, NLAY
                      IF (CHANI(I).GT.0.0) THEN
                        WRITE(IOUT,117)
          117           FORMAT(/,&
             ' ERROR: WHEN A HANI PARAMETER IS USED, CHANI FOR ALL LAYERS',/,&
             ' MUST BE LESS THAN OR EQUAL TO 0.0 -- STOP EXECUTION',&
             ' (GWF2LPFU1AR)')
                        CALL USTOP(' ')
                      end if
          118       CONTINUE
                    NPHANI=1
                 ELSE IF(PTYP.EQ.'VKCB') THEN
                    NPVKCB=1
                 ELSE IF(PTYP.EQ.'VK') THEN
                    NPVK=1
                    CALL SGWF2LPFU1CK(IOUT,N,'VK  ')
                 ELSE IF(PTYP.EQ.'VANI') THEN
                    NPVANI=1
                    CALL SGWF2LPFU1CK(IOUT,N,'VANI')
                 ELSE IF(PTYP.EQ.'SS') THEN
                    NPSS=1
                 ELSE IF(PTYP.EQ.'SY') THEN
                    NPSY=1
                 ELSE
                    WRITE(IOUT,*) ' Invalid parameter type for LPF Package'
                    CALL USTOP(' ')
                 END IF
          120    CONTINUE
              END IF
        !
        !7------READ PARAMETERS AND CONVERT FOR UNSTRUCTURED AND STRUCTURED GRIDS
              IF(IUNSTR.EQ.0) THEN
                CALL SGWF2LPFU1S(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB,&
                 STOTXT,NOPCHK)
              ELSE
                CALL SGWF2LPFU1G(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB,&
                 STOTXT,NOPCHK)
              end if
              modflow.GWF.Kh=hk
        
!        !--------------------------------------------------------------------------------
!        !8------SET INITIAL  GRID-BLOCK SATURATED THICKNESS FRACTIONS AND TRANSMISSIVITY WHEN NEEDED
!              DO K=1,NLAY
!                IF(LAYCON(K).EQ.4.OR.LAYCON(K).EQ.5) THEN
!        !8A-------SET INITIAL SATURATED GRID-BLOCK FRACTIONS FOR LAYCON=4
!                  NNDLAY = NODLAY(K)
!                  NSTRT = NODLAY(K-1)+1
!                  DO N=NSTRT,NNDLAY
!                    IF(IBOUND(N).NE.0) THEN
!        !-------------CALCULATE SATURATED THICKNESS/TOTAL THICKNESS.
!                      HD=HNEW(N)
!                      BBOT=BOT(N)
!                      TTOP=TOP(N)
!                      TOTTHICK = TTOP - BBOT
!                      CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK,K,TTOP)
!                      Sn(N)=THCK
!                      So(N) = Sn(N)
!                    end if
!                  end do
!                end if
!              end do
!        !--------------------------------------------------------------------
!        !9------SET CONSTANT TERMS IN PGF ARRAY IF IT IS NOT READ DIRECTLY
!              IF(IKCFLAG.EQ.0)THEN
!        !9A--------CHECK CV CONSISTENCY
!                CALL SGWF2LPFU1N
!        !
!        !10--------FILL PGF ARRAY
!        !
!        !10A--------FILL VERTICAL TERMS INTO PGF
!                IF(NLAY.GT.1) CALL SGWF2LPFU1VCOND
!        !
!        !10B------FILL HORIZONTAL TERMS INTO PGF - HY FOR LAYCON 4 AND T FOR LAYCONS 0 OR 2
!                CALL FILLPGFH
!              end if
!        !
!        !-----------------------------------------------------------------------------------
!        !11------SET UP STORAGE CAPACITIES FROM COEFFICIENTS
!              IF(ITRSS.NE.0)THEN
!                IF(ISFAC.EQ.0) THEN
!                  CALL SGWF2LPFU1SC(SC1(1),1)
!                ELSE
!                  CALL SGWF2LPFU1SC(SC1(1),0)
!                END IF
!                IF(NCNVRT.GT.0) THEN
!                  CALL SGWF2LPFU1SC(SC2(1),0)
!                end if
!              END IF
!        !
!        !--------------------------------------------------------------------------------
!        !12-----DEALLOCATE UNWANTED ARRAYS
!        !sp need cv for merging Kv with boundary leakance for RIV      DEALLOCATE(CV)
!        !------NEED HK FOR CONDUIT CELLS SO KEEP
!        !      ILAYCON13=0
!        !      DO I=1,NLAY
!        !        IF(LAYCON(I).EQ.1.OR.LAYCON(I).EQ.3)ILAYCON13=1
!        !      end do
!        !      IF(ILAYCON13.EQ.0)THEN
!        !        DEALLOCATE(HK)
!        !      end if
        !13-----RETURN
              RETURN
    END SUBROUTINE ReadLPF
 
    SUBROUTINE ReadCLN_IBOUND_IHEADS(Modflow)
!     ******************************************************************
!     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR CLN
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,INCLN,&
      IDPIN
      USE GWFBASMODULE, ONLY: HNOFLO
      
      implicit none

      type (ModflowProject) Modflow

      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /' CONDUIT BOUNDARY ARRAY'/
      DATA ANAME(2) /'   CONDUIT INITIAL HEAD'/
      
      integer :: n
      
      iout=FNumEco
      INCLN=Modflow.iCLN
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)INCLN
    1 FORMAT(1X,/1X,'CLN -- CONDUIT DOMAIN FLOW PACKAGE, VERSION 1,',&
       ' 5/17/2010 INPUT READ FROM UNIT ',I4)
!
!2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+1),ANAME(1),NCLNNDS,0,INCLN,IOUT)
!3-------READ INITIAL HEADS FOR CLN NODES
      IF(IDPIN.EQ.0) THEN  !----------------------------------SINGLE PRECISION READ
      ALLOCATE(HTMP1(NCLNNDS))
      CALL U1DREL(HTMP1,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP1(N)
        STRT(NODES+N) = HTMP1(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      end do
      DEALLOCATE(HTMP1)
      ELSE    !----------------------------------DOUBLE PRECISION READ
      ALLOCATE(HTMP18(NCLNNDS))
      CALL U1DREL8(HTMP18,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP18(N)
        STRT(NODES+N) = HTMP18(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      end do
      DEALLOCATE(HTMP18)
      end if
      
      do n=1,NCLNNDS
        modflow.CLN.ibound(n)=ibound(NODES+n)
        modflow.CLN.hnew(n)=hnew(NODES+n)
      end do
!!
!!4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
!      DO  IFN=1,NCLNNDS
!        N = ACLNNDS(IFN,1)
!        IFLIN = IFLINCLN(IFN)
!        IF(IBOUND(N).NE.0.AND.IFLIN.LE.0) THEN
!!---------CALCULATE INITIAL SATURATED THICKNESS FOR UNCONFINED CASES.
!          HD=HNEW(N)
!          BBOT = ACLNNDS(IFN,5)
!          CALL CLN_THIK(IFN,HD,BBOT,THCK)
!          Sn(N)=THCK
!          So(N) = Sn(N)
!        end if
!      end do
!!--------------------------------------------------------------------------------
!!5-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
!      CALL SFILLPGF_CLN
!!----------------------------------------------------------------------------------------
!!12A------ESTABLISH WADI CONDITION FOR CLN
!        IWADICLN = 0
!        DO I = 1,NCLNNDS
!           IF(ICCWADICLN(I).NE.0) IWADICLN = 1
!        end do
!        DO I = 1,NCLNGWC
!          IF(ICGWADICLN(I).NE.0) IWADICLN = 1
!        end do
!        IF(IWADICLN.EQ.1) IWADI = 1
!
!6------RETURN
      RETURN
    END SUBROUTINE ReadCLN_IBOUND_IHEADS

    SUBROUTINE ReadSWF_IBOUND_IHEADS(Modflow)
!     ******************************************************************
!     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR SWF
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!      USE CLN1MODULE, ONLY: NCLNNDS,NCLNGWC,ACLNNDS,IFLINCLN,
!     1    ICCWADICLN,ICGWADICLN
      USE SWF1MODULE
      USE CLN1MODULE, ONLY:NCLNNDS
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,INSWF,&
         IDPIN,IUNSTR
      USE GWFBASMODULE, ONLY: HNOFLO

      implicit none

      type (ModflowProject) Modflow

      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'     SWF BOUNDARY ARRAY'/
      DATA ANAME(2) /'       SWF INITIAL HEAD'/
      
      integer :: k, n
      
      iout=FNumEco
      INSWF=Modflow.iSWF
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1) INSWF
    1 FORMAT(1X,/1X,'SWF -- SURFACE DOMAIN FLOW PACKAGE, VERSION 1,',&
       ' 5/17/2010 INPUT READ FROM UNIT ',I4)
!===
!-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
      ALLOCATE(CL12_SWF(NJA_SWF),FAHL_SWF(NJA_SWF))
      IF(INSWF.NE.0) THEN
        IF(IUNSTR.EQ.0) THEN
!          CALL FILLGFS_SWF(IOUT)
        ELSE
!          CALL FILLGFU_SWF(INSWF,IOUT)
          CALL U1DREL(CL12_SWF,ANAME(1),NJA_SWF,K,INSWF,IOUT)
        end if
      end if
      CALL U1DREL(FAHL_SWF,ANAME(1),NJA_SWF,K,INSWF,IOUT)
      !CALL SFILLPGF_SWF
!
!2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+NCLNNDS+1),ANAME(1),NSWFNDS,0,INSWF,IOUT)
!3-------READ INITIAL HEADS FOR CLN NODES
      IF(IDPIN.EQ.0) THEN  !----------------------------------SINGLE PRECISION READ
          ALLOCATE(HTMP1(NSWFNDS))
          CALL U1DREL(HTMP1,ANAME(2),NSWFNDS,0,INSWF,IOUT)
          DO N=1,NSWFNDS
            HNEW(NODES+NCLNNDS+N) = HTMP1(N)
            STRT(NODES+NCLNNDS+N) = HTMP1(N)
            IF(IBOUND(NODES+NCLNNDS+N).EQ.0) HNEW(NODES+NCLNNDS+N)=HNOFLO
          end do
          DEALLOCATE(HTMP1)
      ELSE    !----------------------------------DOUBLE PRECISION READ
          ALLOCATE(HTMP18(NSWFNDS))
          CALL U1DREL8(HTMP18,ANAME(2),NSWFNDS,0,INSWF,IOUT)
          DO N=1,NSWFNDS
            HNEW(NODES+NCLNNDS+N) = HTMP18(N)
            STRT(NODES+NCLNNDS+N) = HTMP18(N)
            IF(IBOUND(NODES+NCLNNDS+N).EQ.0) HNEW(NODES+NCLNNDS+N)=HNOFLO
          end do
          DEALLOCATE(HTMP18)
      end if
      
      do n=1,NSWFNDS
        modflow.SWF.ibound(n)=ibound(NODES+NCLNNDS+n)
        modflow.SWF.hnew(n)=hnew(NODES+NCLNNDS+n)
      end do
!!
!!4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
!      DO  IFN=1,NSWFNDS
!        N = ASWFNDS(IFN,1)
!!        IFLIN = IFLINCLN(IFN)
!        IF(IBOUND(N).NE.0) THEN
!!---------CALCULATE INITIAL SATURATED THICKNESS FOR UNCONFINED CASES.
!          HD=HNEW(N)
!          BBOT = ASWFNDS(IFN,4)
!          CALL SWF_THIK(HD,BBOT,THCK)
!          Sn(N) = THCK
!          So(N) = Sn(N)
!        end if
!      end do
!!--------------------------------------------------------------------------------
!!12A------ESTABLISH WADI CONDITION FOR CLN
!        IWADISWF = 0
!        DO I = 1,NSWFNDS
!           IF(ISSWADISWF(I).NE.0) IWADISWF = 1
!        end do
!        DO I = 1,NSWFGWC
!          IF(ISGWADISWF(I).NE.0) IWADISWF = 1
!        end do
!        IF(IWADISWF.EQ.1) IWADI = 1
!!
!!6------RETURN
      RETURN
      END SUBROUTINE ReadSWF_IBOUND_IHEADS 

    
      SUBROUTINE ReadWEL(Modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFWELMODULE, ONLY:NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,NPWEL,&
                            IWELPB,NNPWEL,WELAUX,WELL,IWELQV,NNPWCLN,&
                            IAFR,IWELLBOT,WELLBOT,NAUXWEL
      implicit none
      
      type (ModflowProject) Modflow
!
      CHARACTER*400 LINE
      
      integer :: in, mxpw, mxactw, lloc
      real :: r
      integer :: istart, istop, n, lstsum, k, lstbeg, ip, numinst, nlst
      integer :: ninlst, i
      
        in=modflow.iWEL
        iout=FNumEco

!     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,IAFR,NAUXWEL)
      ALLOCATE(NPWEL,IWELPB,NNPWEL,IWELQV,NNPWCLN,IWELLBOT)
!
!1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'WEL -- WELL PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
      NNPWCLN=0
      IWELQV=0
      IAFR=0
      IWELLBOT = 0
!
!2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
!2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      WRITE(IOUT,9) MXACTW,IWELCB
    9 FORMAT(1X,'MAXIMUM NUMBER OF ACTIVE WELLS (MXACTW) =',I7&
       /1X,'C-B-C FLUX FLAG OR UNIT NUMBER (IWELCB) =',I4)
!
!3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUXWEL=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
             LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUXWEL.LT.20) THEN
            NAUXWEL=NAUXWEL+1
            WELAUX(NAUXWEL)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUXWEL)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'AUTOFLOWREDUCE') THEN
         WRITE(IOUT,16)
   16    FORMAT(1X,'WELL FLUX WILL BE REDUCED WHEN SATURATED ',&
            'THICKNESS IS LESS THAN 1 PERCENT OF CELL THICKNESS')
         IWELQV = 1
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IUNITAFR') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IAFR,R,IOUT,IN)
         WRITE(IOUT,25) IAFR
   25    FORMAT(1X,'WELL REDUCTION INFO WILL BE WRITTEN TO UNIT: ',&
            I5)
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'WELLBOT') THEN
         IWELLBOT = 1
         WRITE(IOUT,27)
   27    FORMAT(1X,'BOTTOM ELEVATIONS OF WELLS ARE READ',&
            1X,'(WELLBOT OPTION)')
         GO TO 10
      END IF
!3A-----THERE ARE FIVE INPUT VALUES PLUS ONE LOCATION FOR
!3A-----CELL-BY-CELL FLOW BESIDES AUX VARIABLES.
      IF(IWELLBOT.EQ.0) THEN
        NWELVL=5+NAUXWEL
      ELSE
        NWELVL=6+NAUXWEL
      end if
!
!4------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW+MXPW
      ALLOCATE (WELL(NWELVL,MXWELL))
      ALLOCATE (WELLBOT(2,MXWELL))  ! 1 IS FOR WELL BOTTOM; 2 IS FOR 0.01 * THICKNESS FROM THIS BOTTOM
!
!5------READ NAMED PARAMETERS.
      WRITE(IOUT,18) NPWEL
   18 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        LSTSUM=IWELPB
        DO 120 K=1,NPWEL
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',1,&
                        NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
!5A-----READ PARAMETER WITHOUT INSTANCES.
            IF(IUNSTR.EQ.0)THEN
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
              ELSE
                CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,5,5,IPRWEL)
              end if
            ELSE
              IF(IWELLBOT.EQ.0)THEN
               CALL ULSTRDU(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
              IOUT,'WELL NO.      NODE       STRESS FACTOR',&
              WELAUX,20,NAUXWEL,IFREFM,NEQS,4,4,IPRWEL)
              ELSE
               CALL ULSTRDU(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
              IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
              WELAUX,20,NAUXWEL,IFREFM,NEQS,5,5,IPRWEL)
              end if
            end if
          ELSE
!5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRWEL)
            IF(IUNSTR.EQ.0)THEN
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
              ELSE
                CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,5,5,IPRWEL)
              end if
            ELSE
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRDU(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.      NODE       STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NEQS,4,4,IPRWEL)
              ELSE
                CALL ULSTRDU(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NEQS,5,5,IPRWEL)
              end if
            end if
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
!
!6------RETURN
      RETURN
      END SUBROUTINE ReadWEL

      SUBROUTINE SWF_THIK(HD,BBOT,THCK)
!     ******************************************************************
!     COMPUTE FRACTION OF TOTAL VOLUME THAT IS SATURATED
!     FOR CONDUIT NODE AND STORE IN THCK -
!     FRACTION SATURATED DEPENDS ON CONDUIT ORIENTATION
!     ******************************************************************
!
!      SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE SWF1MODULE
      DOUBLE PRECISION THCK,HD,BBOT,&
       DEPTH,SATEPS,D0S0
      
      integer :: ioption
      real :: epsilon
!     ------------------------------------------------------------------
      EPSILON = 0.1D0
      SATEPS = 0.9D0*EPSILON
      D0S0=EPSILON/SATEPS
!
      DEPTH = HD-BBOT
      DEPTH = MAX(MIN_DEPTH,DEPTH)
      ioption=1
      if(ioption==1) then
!    1: Polynomial option
      IF(DEPTH>=EPSILON) THEN
          THCK = SATEPS+(DEPTH-EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = (DEPTH/EPSILON)**2.0D0**((3.0D0*SATEPS-EPSILON) &
               +(EPSILON-2.0D0*SATEPS)*DEPTH/EPSILON)
      end if
      elseif(ioption==2) then
!    2: Simple option       (S0 is fixed to be d0/2)
      IF(DEPTH>=EPSILON) THEN
          THCK = (DEPTH-0.5d0*EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = 1.0d0/2.0d0/EPSILON*DEPTH**2.0D0
      end if
      elseif(ioption==3) then
!    3: Power option
      IF(DEPTH>=EPSILON) THEN
          THCK = SATEPS+(DEPTH-EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = SATEPS/(EPSILON**D0S0)*(DEPTH**D0S0)
      end if
      else
      continue
      end if

      RETURN
!C5------RETURN.
      RETURN
      END SUBROUTINE SWF_THIK
 
      SUBROUTINE UPARLSTRP(LSTSUM,MXLST,IN,IOUT,NP,PACK,PTYPX,ITERP,&
                          NUMINST)
!     ******************************************************************
!     Read and store list parameter definition information for one
!     parameter.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PACK,PTYPX
      CHARACTER*4 PTYP
      CHARACTER*10 PN,CTMP1,CTMP2
      CHARACTER*400 LINE
      
      integer :: in, np, iout,lstsum,iterp, mxlst, numinst, lloc, istart, istop,n
      real :: r, pv
      integer :: nlst, ni
!     ------------------------------------------------------------------
!
!1------Read the parameter name and definition.
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL UPCASE(CTMP1)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLST,R,IOUT,IN)
!
!2------Check for multiple instances.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
        IF (NUMINST.LT.1) THEN
          WRITE(IOUT,12) PN,PTYP
   12     FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',&
             ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,&
             '" -- STOP EXECUTION (UPARLSTRP)')
          CALL USTOP(' ')
        end if
      ELSE
        NUMINST = 0
      end if
!
!3------Look for parameter in list of parameters.
      DO 10 NP=1,MXPAR
        CTMP2=PARNAM(NP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
!
!3A-----If found, determine if it is an illegal duplicate.
          IF(PARTYP(NP).NE.' ' .AND. ITERP.EQ.1) THEN
!
!3B-----Illegal duplicate.
            WRITE(IOUT,110) CTMP1
  110       FORMAT (' Duplicate parameter name: ',A)
            CALL USTOP(' ')
          END IF
!
!3C-----Parameter was predefined in SEN file (PARTYP blank) or in
!3C-----a prior simulation (ITERP not 1) -- leave its value alone.
!3C-----(i.e. ignore PV).
          GO TO 100
        ELSE IF(PARNAM(NP).EQ.' ') THEN
!
!4------Parameter was not found in the list, so it is a new definition.
          PARNAM(NP)=PN
          B(NP)=PV
          IPSUM=IPSUM+1
          GO TO 100
        end if
10    CONTINUE
!
!5------Too many parameters.
      WRITE(IOUT,99)
  99  FORMAT(' Number of parameters exceeds MXPAR -- STOP EXECUTION')
      CALL USTOP(' ')
!
!6------Parameter is a new parameter, or it was predefined in the
!6------Parameter Value file or defined in a previous simulation.  Continue
!6------processing.
 100  CONTINUE
      IF(ITERP.EQ.1) THEN
!
!7------Parameter is new or was predefined in the Parameter Value file.
!7------Process the remaining parameter information.
        PARTYP(NP)=PTYP
        IPLOC(1,NP)=LSTSUM
        NI=MAX(1,NUMINST)
        LSTSUM=LSTSUM+(NLST*NI)
        IPLOC(2,NP)=LSTSUM-1
        IPLOC(3,NP)=NUMINST
        IPLOC(4,NP)=INAMLOC
        INAMLOC=INAMLOC+NUMINST
!
!8------WRITE PARAMETER INFORMATION
        WRITE(IOUT,121) PARNAM(NP),PARTYP(NP)
  121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A)
        WRITE(IOUT,122) PV
  122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
        IF(B(NP).NE.PV) THEN
          WRITE(IOUT,123) B(NP)
  123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,&
             ', as read from',/,' the Parameter Value file')
        END IF
        WRITE(IOUT,130) NLST
  130   FORMAT(  '   NUMBER OF ENTRIES: ',I6)
        IF(NUMINST.GT.0) THEN
          WRITE(IOUT,131)NUMINST
  131     FORMAT('   NUMBER OF INSTANCES: ',I4)
        end if
!
!9------Check if the parameter list will fit in the package list array.
        IF((LSTSUM-1) .GT. MXLST) THEN
          WRITE(IOUT,134) LSTSUM-1,MXLST
  134     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF LIST ENTRIES:'/&
               1X,I5,' list entries have been specified'/&
               1X,'The maximum number of list entries is',I5)
          CALL USTOP(' ')
        END IF
!
!10-----Check if number of instances exceeds the maximum allowed.
        IF((INAMLOC-1).GT.MXINST) THEN
          WRITE(IOUT,135)INAMLOC-1,MXINST
  135     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/&
               1X,I5,' instances have been specified'/&
               1X,'The maximum number of instances is',I5)
          CALL USTOP(' ')
        END IF
!
!11-----Check for correct parameter type.
        IF(PARTYP(NP).NE.PTYPX) THEN
          WRITE(IOUT,137) PTYPX,PACK
  137     FORMAT(1X,'Parameter type must be:',A,' in the ',A,' Package')
          CALL USTOP(' ')
        END IF
!
!12-----Parameter definition must include at least one cell.
        IF (NLST.LE.0) THEN
          WRITE(IOUT,140) PN
          CALL USTOP(' ')
        end if
  140   FORMAT(' ERROR:  DEFINITION FOR PARAMETER "',A,'"',&
           ' INCLUDES NO CELLS',/,'   -- STOP EXECUTION (UPARLSTRP)')
      ELSE
!
!13-----This is not the first time the simulation was run, so the parameter
!13-----was already defined.  Set values of arguments to be returned.
        LSTSUM=LSTSUM+IPLOC(2,NP)-IPLOC(1,NP)+1
        NUMINST=IPLOC(3,NP)
      end if
!
!14-----Set the parameter to be inactive.
      IACTIVE(NP)=0
!
!15-----Return.
      RETURN
    END SUBROUTINE UPARLSTRP

    SUBROUTINE ULSTRD(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,&
          LABEL,CAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ISCLOC1,ISCLOC2,&
          IPRFLG)
!     ******************************************************************
!     Read and print a list.  NAUX of the values in the list are
!     optional -- auxiliary data.
!     ******************************************************************
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      real :: RLIST(LDIM,MXLIST)
      CHARACTER*400 LINE,FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: nlist, lstbeg, iout, inpack, ial, iscloc1, naux
      integer :: nrow, iscloc2, ifrefm, ncol, nlay, iprflg, in, iclose
      real :: sfac
      integer :: lloc, i
      real :: r
      integer :: istop, istart, n, nread2, nread1, ii, jj, k, j
      integer :: mxlist, ldim, idum, iloc, ncaux
      
!     ------------------------------------------------------------------
!
!1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
!
!2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(IPRFLG.EQ.1)WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(IPRFLG.EQ.1)WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE
      END IF
!
!3------Check for SFAC record.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF(IPRFLG.EQ.1) THEN
           WRITE(IOUT,116) SFAC
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.ISCLOC2) THEN
              WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',&
                I2,'-',I2,')')
           END IF
         end if
         READ(IN,'(A)') LINE
      END IF
!
!3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
!
!4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
!
!5------Read the list.
      DO 250 II=LSTBEG,N
!
!5A-----Read a line into the buffer.  (The first line has already been
!5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
!
!5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10,9F10.0)') K,I,J,(RLIST(JJ,II),JJ=4,NREAD1)
         LLOC=10*NREAD1+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
         DO 200 JJ=4,NREAD1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
200      CONTINUE
      END IF
      RLIST(1,II)=K
      RLIST(2,II)=I
      RLIST(3,II)=J
!
!5C------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
204   CONTINUE
!
!5D-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
         DO 210 JJ=NREAD1+1,NREAD2
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
210      CONTINUE
      END IF
!
!5E-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.1)&
         WRITE(IOUT,205) NN,K,I,J,(RLIST(JJ,II),JJ=4,NREAD2)
205   FORMAT(1X,I6,I7,I7,I7,26G16.4)
!
!5F-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT. NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
!
!6------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
!
      RETURN
    END SUBROUTINE ULSTRD
          
    SUBROUTINE ULSTRDU(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,&
          LABEL,CAUX,NCAUX,NAUX,IFREFM,NODES,ISCLOC1,ISCLOC2,&
          IPRFLG)
!     ******************************************************************
!     Read and print a list for unstructured grid variables.
!      NAUX of the values in the list are Optional -- auxiliary data.
!     ******************************************************************
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      real :: RLIST(LDIM,MXLIST)
      CHARACTER*400 LINE,FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: inpack, iout, ial, lstbeg, nlist, iscloc1, nodes
      integer :: naux, ifrefm, iscloc2, iprflg, in, iclose
      real :: sfac
      integer :: lloc, istop
      real :: r
      integer :: i, istart, n, nread2, nread1, ii,k,jj, idum, iloc
      integer :: mxlist, ldim, ncaux
      
!     ------------------------------------------------------------------
!
!1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
!
!2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(IPRFLG.EQ.1)WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(IPRFLG.EQ.1)WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE
      END IF
!
!3------Check for SFAC record.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF(IPRFLG.EQ.1) THEN
           WRITE(IOUT,116) SFAC
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.ISCLOC2) THEN
              WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',&
                I2,'-',I2,')')
           END IF
         end if
         READ(IN,'(A)') LINE
      END IF
!
!3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
!
!4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
!
!5------Read the list.
      DO 250 II=LSTBEG,N
!
!5A-----Read a line into the buffer.  (The first line has already been
!5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
!
!5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10,9F10.0)') K,(RLIST(JJ,II),JJ=4,NREAD1)
         LLOC=10*NREAD1+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         DO 200 JJ=4,NREAD1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
200      CONTINUE
      END IF
      RLIST(1,II)=K
      RLIST(2,II)=1
      RLIST(3,II)=1
!
!5C------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
204   CONTINUE
!
!5D-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
         DO 210 JJ=NREAD1+1,NREAD2
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
210      CONTINUE
      END IF
!
!5E-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.1) &
          WRITE(IOUT,205) NN,K,(RLIST(JJ,II),JJ=4,NREAD2) 
205  FORMAT(1X,I8,5X,I8,26G16.4)
!
!5F-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NODES) THEN
         WRITE(IOUT,*) ' Node number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
!
!6------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
!
      RETURN
    END SUBROUTINE ULSTRDU

    SUBROUTINE ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
!     ******************************************************************
!     PRINT A LABEL FOR A LIST
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      CHARACTER*400 BUF
      CHARACTER*1 DASH(400)
      DATA DASH/400*'-'/
      
      integer :: ncaux, naux, iout, len, nbuf, i, n1, j
!     ------------------------------------------------------------------
!
!1------Construct the complete label in BUF.  Start with BUF=LABEL.
      BUF=LABEL
!
!2------Add auxiliary data names if there are any.
      NBUF=LEN(LABEL)+9
      IF(NAUX.GT.0) THEN
         DO 10 I=1,NAUX
         N1=NBUF+1
         NBUF=NBUF+16
         BUF(N1:NBUF)=CAUX(I)
10       CONTINUE
      END IF
!
!3------Write the label.
      WRITE(IOUT,103) BUF(1:NBUF)
  103 FORMAT(1X,A)
!
!4------Add a line of dashes.
      WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  104 FORMAT(1X,400A)
!
!5------Return.
      RETURN
    END SUBROUTINE ULSTLB
    
    SUBROUTINE ReadCHD(Modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR TIME-VARIANT SPECIFIED-HEAD CELLS AND
!     READ NAMED PARAMETER DEFINITIONS
!     ******************************************************************
!
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFCHDMODULE,ONLY:NCHDS,MXCHD,NCHDVL,IPRCHD,NPCHD,ICHDPB,&
                           NNPCHD,CHDAUX,CHDS
      implicit none
      
      type (ModflowProject) Modflow

      CHARACTER*400 LINE
      
      integer :: in, mxpc, mxactc, lloc
      real :: r
      integer :: istart, istop, naux, n, lstsum, k, lstbeg, ip, numinst, nlst, ib, lb, i

      in=modflow.iCHD 
      iout=FNumEco
!     ------------------------------------------------------------------
      ALLOCATE(NCHDS,MXCHD,NCHDVL,IPRCHD)
      ALLOCATE(NPCHD,ICHDPB,NNPCHD)
!
!1------IDENTIFY OPTION AND INITIALIZE # OF SPECIFIED-HEAD CELLS
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'CHD -- TIME-VARIANT SPECIFIED-HEAD OPTION,',&
       ' VERSION 7, 5/2/2005',/1X,'INPUT READ FROM UNIT ',I4)
      NCHDS=0
      NNPCHD=0
!
!2------READ AND PRINT MXCHD (MAXIMUM NUMBER OF SPECIFIED-HEAD
!2------CELLS TO BE SPECIFIED EACH STRESS PERIOD)
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPCHD,MXPC)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10)') MXACTC
         LLOC=11
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTC,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTC
    3 FORMAT(1X,'MAXIMUM OF ',I6,&
       ' ___TIME-VARIANT SPECIFIED-HEAD CELLS AT ONE TIME')
!
!3------READ AUXILIARY VARIABLES AND PRINT OPTION
      ALLOCATE (CHDAUX(100))
      NAUX=0
      IPRCHD=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
             LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.100) THEN
            NAUX=NAUX+1
            CHDAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) CHDAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY CHD VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,&
     'LISTS OF TIME-VARIANT SPECIFIED-HEAD CELLS WILL NOT BE PRINTED')
         IPRCHD = 0
         GO TO 10
      END IF
      NCHDVL=5+NAUX
!
!4------ALLOCATE SPACE FOR TIME-VARIANT SPECIFIED-HEAD LIST.
      ICHDPB=MXACTC+1
      MXCHD=MXACTC+MXPC
      ALLOCATE (CHDS(NCHDVL,MXCHD))
!
!1------READ NAMED PARAMETERS.
      WRITE(IOUT,1000) NPCHD
 1000 FORMAT(1X,//1X,I5,' TIME-VARIANT SPECIFIED-HEAD PARAMETERS')
      IF(NPCHD.GT.0) THEN
        NAUX=NCHDVL-5
        LSTSUM=ICHDPB
        DO 120 K=1,NPCHD
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXCHD,IN,IOUT,IP,'CHD','CHD',1,NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
!         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          end if
!         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 110 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,IPRCHD)
            end if
            IF(IUNSTR.EQ.0)THEN
              CALL ULSTRD(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,&
          'CHD NO.   LAYER   ROW   COL   START FACTOR      END FACTOR',&
             CHDAUX,100,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,IPRCHD)
            ELSE
              CALL ULSTRDU(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,&
          'CHD NO.        NODE           START FACTOR      END FACTOR',&
             CHDAUX,100,NAUX,IFREFM,NEQS,4,5,IPRCHD)
            end if
            LB=LB+NLST
  110     CONTINUE
  120   CONTINUE
      END IF
!
!3------RETURN.
      RETURN
    END SUBROUTINE ReadCHD
    
    
      SUBROUTINE ReadRCH(modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR RECHARGE
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IFREFM,NODLAY,IUNSTR
      USE GWFBASMODULE, ONLY: IATS
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,NPRCH,IRCHPF,RECH,IRCH,&
       MXNDRCH,INIRCH,NIRCH,SELEV,iznrch,mxznrch,ISELEV,IPONDOPT,&
       IRTSOPT,ICONCRCHOPT,INRTS,TIMRCH,IRTSRD,RECHSV,RCHF,&
       RCHCONC,IRCHCONC,ICONCRCHOPT
      USE GWTBCTMODULE, ONLY: MCOMPT

      implicit none
      
      type (ModflowProject) Modflow

!
      CHARACTER*400 LINE
      CHARACTER*4 PTYP
      
      integer :: in, lloc, istart, istop
      real :: r
      integer :: n, inoc, i, iconcrch, ii, inbct, inselev, k
      
      in=modflow.iRCH 
      iout=FNumEco
      
      INBCT=0   !rgm for now assume no bct (block-centred transport) 


!     ------------------------------------------------------------------
!
!1-------ALLOCATE SCALAR VARIABLES.
      ALLOCATE(NRCHOP,IRCHCB,MXNDRCH)
      ALLOCATE(NPRCH,IRCHPF,INIRCH,NIRCH)
      ALLOCATE(INRTS,TIMRCH,IRTSRD)
      IRTSRD=0
!
!2------IDENTIFY PACKAGE.
      IRCHPF=0
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
!
!3------READ NRCHOP AND IRCHCB.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARARRAL(IN,IOUT,LINE,NPRCH)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NRCHOP,IRCHCB
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRCHOP,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRCHCB,R,IOUT,IN)
      END IF
!
!3B------READ KEYWORD OPTIONS SEEPELEV, RTS AND RECHARGE CONC.
      ALLOCATE(IPONDOPT,IRTSOPT,ICONCRCHOPT)
      IPONDOPT=0
      IRTSOPT=0
      ICONCRCHOPT=0
      LLOC=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
!3B1------FOR SEEPAGE-FACE ELEVATION
      IF(LINE(ISTART:ISTOP).EQ.'SEEPELEV') THEN
        WRITE(IOUT,13)
   13   FORMAT(1X,'SEEPAGE-FACE ELEVATIONS WILL BE READ.',&
           '  VARIABLE INSELEV REQUIRED IN RECORD 5.')
        IPONDOPT = 1
      END IF
!3B2------FOR RTS
      IF(LINE(ISTART:ISTOP).EQ.'RTS') THEN
!3B2A-----CHECK TO SEE IF ATS IS ON. OR ELSE WRITE WARNING AND STOP
        IF(IATS.EQ.0)THEN
          WRITE(IOUT,15)
          STOP
        end if
15      FORMAT(1X,'TRANSIENT RECHARGE NEEDS ADAPTIVE TIME-STEPPING.',&
          'STOPPING')
!3B2B------SET OPTION, AND READ MAXIMUM NUMBER OF ZONES OF TRANSIENT RCH.
        ALLOCATE(MXZNRCH)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXZNRCH,R,IOUT,INOC)
        WRITE(IOUT,14)MXZNRCH
   14   FORMAT(1X,'TRANSIENT RECHARGE FILE WITH',I8,' ZONES WILL BE',&
           ' READ. RECHARGE ZONE INDICES WILL BE READ FROM RCH FILE.'&
              /1X,107('-'))
        IRTSOPT = 1
      END IF
!3BC------FOR CONCENTRATION OF RECHARGE
      IF(LINE(ISTART:ISTOP).EQ.'CONCENTRATION' .OR.&
        LINE(ISTART:ISTOP).EQ.'CONC' ) THEN
        WRITE(IOUT,16)
   16   FORMAT(1X,'SPECIES CONCENTRATION WILL BE READ.',&
           '  ARRAY IRCHCONC REQUIRED TO INDICATE SPECIES')
        ICONCRCHOPT = 1
      END IF
      IF(LLOC.LT.200) GO TO 10
      
!C3C------READ NUMBER OF RECHARGE NODES IF UNSTRUCTURED AND NRCHOP=2
      IF(IUNSTR.EQ.1.AND.NRCHOP.EQ.2)THEN
        READ(IN,*) MXNDRCH
      ELSEIF(IUNSTR.EQ.1.AND.NRCHOP.EQ.4)THEN
        MXNDRCH = NSWFNDS
      ELSE
        MXNDRCH = NODLAY(1)
      end if
     
      
!3D-----ALLOCATE ZONAL ARRAY
        IF(IRTSOPT.EQ.1)THEN
          ALLOCATE(iznrch(mxndrch))
        ELSE
          ALLOCATE(iznrch(1))
        end if
!3D-----ALLOCATE CONCENTRATION ARRAY
        IF(ICONCRCHOPT.GT.0)THEN
          ALLOCATE(IRCHCONC(MCOMPT))
!3D1------READ INDEX ARRAY FOR COMPONENTS WHOSE CONC IS READ
          READ(IN,*)(IRCHCONC(I),I=1,MCOMPT)
          ICONCRCH = 0
          DO II=1,MCOMPT
            ICONCRCH = ICONCRCH + IRCHCONC(II)
          end do
          ALLOCATE(RCHCONC(mxndrch,ICONCRCH))
!3D1------READ ARRAY OF COMPONENT NUMBERS
        ELSE
          ALLOCATE(IRCHCONC(1))
          ALLOCATE(RCHCONC(1,1))
        end if
!
!4------CHECK TO SEE THAT OPTION IS LEGAL.
      IF(NRCHOP.LT.1.OR.NRCHOP.GT.4) THEN
        WRITE(IOUT,8) NRCHOP
    8   FORMAT(1X,'ILLEGAL RECHARGE OPTION CODE (NRCHOP = ',I5,&
            ') -- SIMULATION ABORTING')
        CALL USTOP(' ')
      END IF
!
!5------OPTION IS LEGAL -- PRINT OPTION CODE.
      IF(NRCHOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- RECHARGE TO TOP LAYER')
      IF(NRCHOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- RECHARGE TO ONE SPECIFIED NODE IN EACH',&
          ' VERTICAL COLUMN')
      IF(NRCHOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- RECHARGE TO HIGHEST ACTIVE NODE IN',&
          ' EACH VERTICAL COLUMN')
      IF(NRCHOP.EQ.4) WRITE(IOUT,205)
  205 FORMAT(1X,'OPTION 4 -- RECHARGE TO SWF DOMAIN ON TOP OF ',&
          ' EACH VERTICAL COLUMN')
!
!6------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF(IRCHCB.GT.0) WRITE(IOUT,204) IRCHCB
  204 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
!
!7------ALLOCATE SPACE FOR THE RECHARGE (RECH) AND INDICATOR (IRCH)
!7------ARRAYS.
      ALLOCATE (RECH(MXNDRCH))
      ALLOCATE (IRCH(MXNDRCH))
!8------IF TRANSPORT IS ACTIVE THEN ALLOCATE ARRAY TO STORE FLUXES
      IF(INBCT.GT.0)THEN
        ALLOCATE (RCHF(MXNDRCH))
      end if
!--------ALLOCATE SPACE TO SAVE SEEPAGE-FACE INFORMATION
      ALLOCATE (ISELEV)
      ISELEV = 0
      INSELEV = 0
      IF (IPONDOPT.GT.0) THEN
        ALLOCATE(SELEV(mxndrch))
        DO I=1,MXNDRCH
          SELEV(I) = 1.0E20
        end do
      ELSE
          ALLOCATE(SELEV(1))
      end if
!---------ALLOCATE SPACE TO SAVE ORIGINAL RECH ARRAY FROM STRESS PERIODS
      IF(IRTSOPT.GT.0)THEN
        ALLOCATE (RECHSV(MXNDRCH))
      ELSE
        ALLOCATE (RECHSV(1))
      end if
!
!8------READ NAMED PARAMETERS
      WRITE(IOUT,5) NPRCH
    5 FORMAT(1X,//1X,I5,' Recharge parameters')
      IF(NPRCH.GT.0) THEN
         DO 20 K=1,NPRCH
         CALL UPARARRRP(IN,IOUT,N,0,PTYP,1,1,0)
         IF(PTYP.NE.'RCH') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be RCH')
            CALL USTOP(' ')
         END IF
   20    CONTINUE
      END IF
!
!9------RETURN
      RETURN
      END SUBROUTINE ReadRCH

!------------------------------------------------------------------
      SUBROUTINE ReadDRN(modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR DRAINS AND READ PARAMETER DEFINITIONS
!     ******************************************************************
!
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFDRNMODULE, ONLY:NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN,NPDRN,&
                            IDRNPB,NNPDRN,DRNAUX,DRAI

      implicit none
      
      type (ModflowProject) Modflow

      integer :: in, mxpd, mxactd, lloc, istart, istop
      real :: r
      integer :: naux, n, lstsum, k, lstbeg, numinst, ip, nlst, ninlst, i
      
      CHARACTER*400 LINE
!     ------------------------------------------------------------------
      ALLOCATE(NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN)
      ALLOCATE(NPDRN,IDRNPB,NNPDRN)

      in=modflow.iDRN 
      iout=FNumEco
!
!1------IDENTIFY PACKAGE AND INITIALIZE NDRAIN.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'DRN -- DRAIN PACKAGE, VERSION 7, 5/2/2005',&
      ' INPUT READ FROM UNIT ',I4)
      NDRAIN=0
      NNPDRN=0
!
!2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
!2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPDRN,MXPD)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTD,IDRNCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTD,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDRNCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTD
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE DRAINS AT ONE TIME')
      IF(IDRNCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
         IF(IDRNCB.GT.0) WRITE(IOUT,8) IDRNCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
!
!3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      ALLOCATE (DRNAUX(20))
      NAUX=0
      IPRDRN=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
             LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            DRNAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) DRNAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY DRAIN VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF DRAIN CELLS WILL NOT BE PRINTED')
         IPRDRN = 0
         GO TO 10
      END IF
!3A-----THERE ARE FIVE INPUT DATA VALUES PLUS ONE LOCATION FOR
!3A-----CELL-BY-CELL FLOW.
      NDRNVL=6+NAUX
!
!4------ALLOCATE SPACE FOR DRAIN ARRAYs.
      IDRNPB=MXACTD+1
      MXDRN=MXACTD+MXPD
      ALLOCATE (DRAI(NDRNVL,MXDRN))
!
!5------READ NAMED PARAMETERS.
      WRITE(IOUT,1000) NPDRN
 1000 FORMAT(1X,//1X,I5,' Drain parameters')
      IF(NPDRN.GT.0) THEN
        LSTSUM=IDRNPB
        DO 120 K=1,NPDRN
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXDRN,IN,IOUT,IP,'DRN','DRN',1,NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
!5A-----READ PARAMETER WITHOUT INSTANCES
            IF(IUNSTR.EQ.0)THEN
                CALL ULSTRD(NLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,&
                    'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR',&
                    DRNAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
            ELSE
                CALL ULSTRDU(NLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,&
                    'DRAIN NO.      NODE         DRAIN EL.  CONDUCTANCE',&
                    DRNAUX,5,NAUX,IFREFM,NEQS,5,5,IPRDRN)
            ENDIF
          ELSE
!5B-----READ INSTANCES
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRDRN)
            IF(IUNSTR.EQ.0)THEN
                CALL ULSTRD(NINLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,&
                    'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR',&
                    DRNAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
            ELSE
                CALL ULSTRDU(NINLST,DRAI,LSTBEG,NDRNVL,MXDRN,1,IN,IOUT,&
                    'DRAIN NO.      NODE         DRAIN EL.  CONDUCTANCE',&
                    DRNAUX,20,NAUX,IFREFM,NEQS,5,5,IPRDRN)
            ENDIF
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
!
!6------RETURN
      RETURN
      end SUBROUTINE ReadDRN

      
!------------------------------------------------------------------
      SUBROUTINE ReadSWBC(modflow)! based on SWF2BC1U1AR
!     ALLOCATE ARRAY STORAGE FOR SWFBC AND READ PARAMETER DEFINITIONS
      USE GLOBAL,      ONLY:IOUT,IFREFM
      USE GLOBAL,      ONLY:ITMUNI,LENUNI
      USE SWFBCMODULE
      
      implicit none
      
      type (ModflowProject) Modflow
      
      CHARACTER*400 LINE
      
      integer :: in, lloc
      real :: r
      integer :: istart, istop
      
      in=modflow.iSWBC 
      iout=FNumEco

      ALLOCATE(NSWBC,NCRD,NZDG,NVSWBC,ISWBCCB)
! GRAVITY CORRECTION
      GRAV_SWF = 9.81D0
      IF(ITMUNI.EQ.0) THEN
          CONTINUE
      ELSE IF(ITMUNI.EQ.1) THEN
          CONTINUE
      ELSE IF(ITMUNI.EQ.2) THEN
          GRAV_SWF=GRAV_SWF*(60.0D0)**2.0D0
      ELSE IF(ITMUNI.EQ.3) THEN
          GRAV_SWF=GRAV_SWF*(3600.0D0)**2.0D0
      ELSE IF(ITMUNI.EQ.4) THEN
          GRAV_SWF=GRAV_SWF*(3600.0D0*24.0D0)**2.0D0
      ELSE
          GRAV_SWF=GRAV_SWF*(3600.0D0*24.0D0*365.0D0)**2.0D0
      END IF
      IF(LENUNI.EQ.0) THEN
          CONTINUE
      ELSE IF(LENUNI.EQ.1) THEN
          GRAV_SWF=GRAV_SWF/0.3048D0
      ELSE IF(LENUNI.EQ.2) THEN
          CONTINUE
      ELSE IF(LENUNI.EQ.3) THEN
          GRAV_SWF=GRAV_SWF/0.01D0
      END IF
!
!1------IDENTIFY PACKAGE AND INITIALIZE NDRAIN.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'SWBC -- SWF BOUNDARY CONDITION - VERSION 1',&
     ' INPUT READ FROM UNIT ',I4)
      NSWBC=0
      NCRD=0
      NZDG=0
      NVSWBC=1  ! LENGTH ASSOCIATED WITH THE NODE, to be stored in VSWBC(1,1:NSWBC)
!
!2------READ THE NUMBER OF SWBC NODES AND UNIT OR FLAG FOR
!2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NSWBC,ISWBCCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWBC,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWBCCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) NSWBC
    3 FORMAT(1X,'SWBC ',I6,' NODES')
      IF(ISWBCCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ISWBCCB NOT 0')
      IF(ISWBCCB.GT.0) WRITE(IOUT,8) ISWBCCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
!
! allocate - nodelist and variables to be used
      ALLOCATE (ISWBC(NSWBC),VSWBC(NVSWBC,NSWBC),QSWBC(NSWBC))
!

!------RETURN
      RETURN
      END SUBROUTINE ReadSWBC
!------------------------------------------------------------------
      SUBROUTINE ReadSWBC2(modflow)! based on SWF2BC1U1RP
!     READ LENGTH ASSOCIATED WITH THE SWBC
      USE GLOBAL,      ONLY:IOUT,IFREFM
      USE SWFBCMODULE

      implicit none
      
      type (ModflowProject) Modflow
      
      integer :: in, np, L
      
      in=modflow.iSWBC 
      iout=FNumEco
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'SWBC -- SWF BOUNDARY CONDITION - VERSION 1',&
     ' INPUT READ FROM UNIT ',I4)
!
!1------READ ITMP (NUMBER OF SWBC OR FLAG TO REUSE DATA) AND
!1------NUMBER OF PARAMETERS.
      !IF(NVSWBC.GT.0) THEN
      !   IF(IFREFM.EQ.0) THEN
      !      READ(IN,'(2I10)') ITMP,NP
      !   ELSE
      !      READ(IN,*) ITMP,NP
      !   END IF
      !ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      !END IF
!
!2------DETERMINE THE NUMBER OF NON-PARAMETER DRAINS.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,&
             'REUSING NON-PARAMETER DRAINS FROM LAST STRESS PERIOD')
      ELSE
         !NNPDRN=ITMP
      END IF
!
!3------IF THERE ARE NEW NON-PARAMETER DRAINS, READ THEM.
!      MXACTD=IDRNPB-1
      IF(ITMP.GT.0) THEN
!         IF(IUNSTR.EQ.0)THEN
!           CALL ULSTRD(NSWBC,ISWBC,1,NVSWBC,NSWBC,1,IN,IOUT,
!     1     'SWBC NO.  LAYER   ROW   COL     SWBC EL.  LENGTH',
!     2     DRNAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
!         ELSE
!YJP           CALL ULSTRDU(NSWBC,ISWBC,1,NVSWBC,NSWBC,1,IN,IOUT,
!     1     'DRAIN NO.      NODE         DRAIN EL.  CONDUCTANCE',
!     2     dummyc16,20,0,IFREFM,NEQS,5,5,0)
!         end if
          DO L=1,NSWBC
              READ(IN,*) ISWBC(L),VSWBC(1,L)
          end do
      END IF
!      NDRAIN=NNPDRN
!
!3------PRINT NUMBER OF DRAINS IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NSWBC
  101 FORMAT(1X,/1X,I6,' SWBCS')
!
!-------FOR STRUCTURED GRID, CALCULATE NODE NUMBER AND PLACE IN LAYER LOCATION
!      IF(ITMP.GT.0.AND.IUNSTR.EQ.0)THEN
!        DO L=1,NNPDRN  ! ONLY NEEDS TO BE DONE FOR NON-PARAMETER DRAINS
!          IR=DRAI(2,L)
!          IC=DRAI(3,L)
!          IL=DRAI(1,L)
!          N = IC + NCOL*(IR-1) + (IL-1)* NROW*NCOL
!          DRAI(1,L) = N
!        end do
!      end if
!
!8------RETURN.
      RETURN
      END SUBROUTINE ReadSWBC2
     
!------------------------------------------------------------------
      SUBROUTINE ReadRCH_StressPeriods(Modflow)
!     READ RECHARGE DATA FOR STRESS PERIOD
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,&
       NODLAY,AREA,IUNSTR,NODES
      USE GWFRCHMODULE,ONLY:NRCHOP,NPRCH,IRCHPF,RECH,IRCH,INIRCH,NIRCH,&
       SELEV,iznrch,mxznrch,ISELEV,IPONDOPT,IRTSOPT,RECHSV,ICONCRCHOPT,&
       tstartrch,tendrch,factrrch,RTSRCH,INRTS,IRTSRD,TIMRCH,&
       RCHCONC,IRCHCONC
      USE GWTBCTMODULE, ONLY: MCOMPT
      
      implicit none
      
      type (ModflowProject) Modflow

      REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
      INTEGER, DIMENSION(:,:),ALLOCATABLE  ::ITEMP
!
      CHARACTER*24 ANAME(5)
      CHARACTER(LEN=200) line
!
      DATA ANAME(1) /'    RECHARGE LAYER INDEX'/
      DATA ANAME(2) /'                RECHARGE'/
      DATA ANAME(3) /'                   SELEV'/
      DATA ANAME(4) /'                  iznrch'/
      DATA ANAME(5) /'                    CONC'/
      
      integer :: in, lloc, iniznrch, inselev, inconc
      real :: r
      integer :: istop, n, istart, inoc, inrech, i, j, ir, ic
      integer :: iflag, kper, iurts, izr, iconcrch, ii
      
      in=modflow.iRCH
      iout=FNumEco
!     ------------------------------------------------------------------
      ALLOCATE (TEMP(NCOL,NROW))
      ALLOCATE (ITEMP(NCOL,NROW))
!2------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
!
!2------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.
      lloc = 1
      iniznrch=0
      INSELEV=0
      INCONC=0
      CALL URDCOM(In, Iout, line)
!3------GET OPTIONS FIRST
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'INRCHZONES') THEN
!3B------READ KEYWORD OPTION FOR RTS ZONES TO BE READ.
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INIZNRCH,R,IOUT,INOC)
        WRITE(IOUT,14) INIZNRCH
14      FORMAT(/1X,'FLAG FOR INPUT OF RTS ZONES (INIZNRCH) = ',&
             I8)
      ELSEIF(LINE(ISTART:ISTOP).EQ.'INSELEV') THEN
!3C------IS KEWORD OPTION FOR SEEPAGE ELEVATION TO BE READ
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INSELEV,R,IOUT,INOC)
        WRITE(IOUT,15) INSELEV
15      FORMAT(/1X,'FLAG FOR INPUT OF SEEPAGE ELEVATIONS (INSELEV) = ',&
             I8)
       ELSEIF(LINE(ISTART:ISTOP).EQ.'INCONC') THEN
!3C------IS KEWORD OPTION FOR CONCENTRATION TO BE READ
        INCONC = 1
        WRITE(IOUT,16) INCONC
16      FORMAT(/1X,'FLAG FOR INPUT OF CONCENTRATIONS (INCONC) = ',&
             I8)
      END IF
      IF(LLOC.LT.200) GO TO 10
      LLOC = 1
!3D------READ FLAGS
      IF(IFREFM.EQ.0)THEN
        IF(NRCHOP.EQ.2) THEN
          READ(LINE,'(2I10)') INRECH,INIRCH
        ELSE
          READ(LINE,'(I10)') INRECH
          INIRCH = NODLAY(1)
        end if
      ELSE
        IF(NRCHOP.EQ.2) THEN
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          CALL URWORD(line, lloc, istart, istop, 2, inirch, r, Iout, In)
        ELSE
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          INIRCH = NODLAY(1)
        end if
      END IF
      IF(INIRCH.GE.0) NIRCH = INIRCH
      IF(INSELEV.GE.0) ISELEV = INSELEV
!
!3------TEST INRECH TO SEE HOW TO DEFINE RECH.
      IF(INRECH.LT.0) THEN
!
!3A-----INRECH<0, SO REUSE RECHARGE ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'REUSING RECH FROM LAST STRESS PERIOD')
      ELSE
        IF(IUNSTR.EQ.0)THEN
!
!3B-----INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
!
!3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(TEMP,ANAME(2),NROW,NCOL,0,IN,IOUT)
          ELSE
!3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
!3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
   33       FORMAT(1X,///1X,&
           'RECH array defined by the following parameters:')
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
   34         FORMAT(' ERROR: When parameters are defined for the RCH',&
           ' Package, at least one parameter',/,' must be specified',&
           ' each stress period -- STOP EXECUTION (GWF2RCH8U1RPLL)')
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(TEMP,NCOL,NROW,0,INRECH,IN,IOUT,'RCH',&
                 ANAME(2),'RCH',IRCHPF)
          END IF
          N=0
          DO I=1,NROW
          DO J=1,NCOL
            N=N+1
            RECH(N)=TEMP(J,I)
          end do
          end do
        ELSE ! READ RECH FOR UNSTRUCTURED GRID
!3B-------INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
!
!3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(RECH,ANAME(2),1,NIRCH,0,IN,IOUT)
          ELSE
!
!3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
!3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(RECH,NIRCH,1,0,INRECH,IN,IOUT,'RCH',&
                 ANAME(2),'RCH',IRCHPF)
          END IF
        end if
      end if
!
!5------IF NRCHOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.  TEST INIRCH
!5------TO SEE HOW TO DEFINE IRCH.
        IF(NRCHOP.EQ.2) THEN
          IF(INIRCH.LT.0) THEN
!
!5A---------INIRCH<0, SO REUSE LAYER INDICATOR ARRAY FROM LAST STRESS PERIOD.
            WRITE(IOUT,2)
    2       FORMAT(1X,/1X,'REUSING IRCH FROM LAST STRESS PERIOD')
          ELSE
!
!5B---------INIRCH=>0, SO CALL U2DINT TO READ LAYER INDICATOR ARRAY(IRCH)
            IF(IUNSTR.EQ.0)THEN
              CALL U2DINT(ITEMP,ANAME(1),NROW,NCOL,0,IN,IOUT)
              N=0
              DO 57 IR=1,NROW
              DO 57 IC=1,NCOL
                N=N+1
                IF(ITEMP(IC,IR).LT.1 .OR. ITEMP(IC,IR).GT. NLAY) THEN
                  WRITE(IOUT,56) IC,IR,ITEMP(IC,IR)
   56             FORMAT(/1X,'INVALID LAYER NUMBER IN IRCH FOR COLUMN',&
                 I4,'  ROW',I4,'  :',I4)
                 CALL USTOP(' ')
                END IF
                IRCH(N) = (ITEMP(IC,IR)-1)*NROW*NCOL + (IR-1)*NCOL + IC
   57         CONTINUE
              NIRCH = NROW*NCOL
            ELSE
              CALL U2DINT(IRCH,ANAME(1),1,NIRCH,0,IN,IOUT)
!----------------------------------------------------
! ------------CHECK FOR IRCH BEING LARGER THAN NODES
              IFLAG = 0
              DO I=1,NIRCH
                IF(IRCH(I).GT.NODES)THEN
                  IFLAG = IRCH(I)
                  GO TO 112
                end if
              end do
112           CONTINUE
! ------------WRITE MESSAGE AND STOP IF IEVT IS LARGER THAN NODES
              IF(IFLAG.GT.0)THEN
                WRITE(IOUT,75)IFLAG,NODES
75              FORMAT('INDEX NODE NO.',I10,&
               ', LARGER THAN TOTAL GWF NODES (',I10,'), STOPPING')
                STOP
              end if
!----------------------------------------------------
            END IF
          END IF
        ELSE ! NRCHOP IS NOT 2 SO SET TOP LAYER OF NODES IN IRCH
          DO I=1,NIRCH
            IRCH(I) = I
          end do
        END IF
!
!-------IF RECHARGE IS READ THEN MULTIPLY BY AREA TO GIVE FLUX
        IF(INRECH.GE.0) THEN
!
!4--------MULTIPLY RECHARGE RATE BY CELL AREA TO GET VOLUMETRIC RATE.
          DO 50 NN=1,NIRCH
            N = IRCH(NN)
            RECH(NN)=RECH(NN)*AREA(N)
   50     CONTINUE
        END IF
!----------------------------------------------------------------
!----------RECHARGE ZONES
      IF(IRTSOPT.EQ.0) GO TO 101
      IF(INiznrch.LE.0) THEN
!
!3A-----INiznrch=<0, SO REUSE iznrch ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,5)
    5   FORMAT(1X,/1X,'REUSING iznrch FROM LAST STRESS PERIOD')
      ELSEif(INiznrch.gt.0)then
!3B-----READ IZNRCH ARRAY AND FIRST TIME OF RTS FILE AT KPER=1
        mxznrch = iniznrch
        IF(IUNSTR.EQ.0)THEN
          CALL U2DINT(iznrch,ANAME(4),NROW,NCOL,0,IN,IOUT)
        ELSE
          CALL U2DINT(iznrch,ANAME(4),1,NIRCH,0,IN,IOUT)
        end if
!3C-------READ FIRST LINE OF RTS FILE AT KPER=1
        IF(KPER.EQ.1)THEN
          inrts = IURTS
          allocate(tstartrch,tendrch,factrrch,rtsrch(mxznrch))
         read(inrts,*)tstartrch,tendrch,factrrch,(rtsrch(i),i=1,mxznrch)
         write(iout,7)tstartrch,tendrch,factrrch,(rtsrch(i),i=1,mxznrch)
7        format(2x,'*** RTS read - Tstart, Tend, Factor, Rts(mxznrch)'/&
          5x,200g15.7)
!3D-------SET FLAGS FOR RTS AND ATS
          TIMRCH = TENDRCH
          IRTSRD = 0
        end if
      end if
!-----------------------------------------------------------------
!4--------APPLY RTS TO RECHARGE ARRAY IF RECHARGE OR ZONES CHANGE
        IF(INRECH.GE.0.OR.INIZNRCH.GT.0)THEN
!---------save original stress-period RECH in RECHSV array for later use
          IF(INRECH.GE.0)THEN
            DO NN=1,NIRCH
              RECHSV(NN) = RECH(NN)
            end do
          end if
!---------Add RTS recharge to RECH already on nodes
          DO 52 NN=1,NIRCH
          N = IRCH(NN)
          izr = iznrch(n)
          if(izr.ge.1.and.izr.le.mxznrch)&
         RECH(NN)=RECHSV(NN) + rtsrch(izr)*AREA(N)*factrrch
   52     CONTINUE
          WRITE(IOUT,6)
6         FORMAT(2X,'*** RECH ARRAY UPDATED FROM RTS FILE ***')
        end if
101   CONTINUE
!----------------------------------------------------------------
!----------UNCONFINED RECHARGE WITHOUT PONDING
      IF(IPONDOPT.EQ.0) GO TO 102
      IF(INSELEV.LE.0) THEN
!
!3A-----INSELEV<0, SO REUSE SELEV ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,4)
    4   FORMAT(1X,/1X,'REUSING SELEV FROM LAST STRESS PERIOD')
      ELSEif(INSELEV.gt.0)then
        IF(IUNSTR.EQ.0)THEN
          CALL U2DREL(SELEV,ANAME(3),NROW,NCOL,0,IN,IOUT)
        ELSE
          CALL U2DREL(SELEV,ANAME(3),1,NIRCH,0,IN,IOUT)
        end if
      end if
102   CONTINUE
!----------------------------------------------------------------
!----------CONCENTRATION OF RECHARGE FOR TRANSPORT
      IF(ICONCRCHOPT.EQ.0) GO TO 103
      IF(INCONC.LE.0) THEN
!
!3A-----INCONC<0, SO REUSE CONCENTRATION ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,8)
    8   FORMAT(1X,/1X,'REUSING CONCENTRATION FROM LAST STRESS PERIOD')
      ELSEif(INCONC.gt.0)then
        ICONCRCH = 0
        DO II=1,MCOMPT
!          WRITE(IOUT,*)(' READING FOR COMPONENT NUMBER',MCOMPT)
          WRITE(IOUT,*) ' READING FOR COMPONENT NUMBER',MCOMPT  !kkz - remove parentheses per JCH (alternative is to use a FORMAT)
          IF(IRCHCONC(II).NE.0)THEN
            ICONCRCH = ICONCRCH + 1
            IF(IUNSTR.EQ.0)THEN
             CALL U2DREL(RCHCONC(1,ICONCRCH),ANAME(5),NROW,NCOL,&
                  0,IN,IOUT)
            ELSE
             CALL U2DREL(RCHCONC(1,ICONCRCH),ANAME(5),1,NIRCH,0,IN,IOUT)
            end if
          end if
        end do
      end if
103   CONTINUE
!---------------------------------------------------------------
      DEALLOCATE(TEMP)
      DEALLOCATE(ITEMP)
!6------RETURN
      RETURN
      END SUBROUTINE ReadRCH_StressPeriods

    SUBROUTINE ReadDRN_StressPeriods(Modflow)
!     ******************************************************************
!     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION
!     ******************************************************************
!
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFDRNMODULE, ONLY:NDRAIN,MXDRN,NDRNVL,IPRDRN,NPDRN,&
                            IDRNPB,NNPDRN,DRNAUX,DRAI
      
      implicit none
      
      type (ModflowProject) Modflow
      
      integer :: in, np, naux, ioutu, mxactd, nread, n, l, ir, ic, il

      in=modflow.iDRN
      iout=FNumEco
      
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'DRN -- DRAIN PACKAGE, VERSION 7, 5/2/2005',&
        ' INPUT READ FROM UNIT ',I4)
!
!1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
!1------NUMBER OF PARAMETERS.
      IF(NPDRN.GT.0) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') ITMP,NP
         ELSE
            READ(IN,*) ITMP,NP
         END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
!
!------CALCULATE SOME CONSTANTS
      NAUX=NDRNVL-6
      IOUTU = IOUT
      IF(IPRDRN.EQ.0) IOUTU=-IOUT
!
!2------DETERMINE THE NUMBER OF NON-PARAMETER DRAINS.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,&
              'REUSING NON-PARAMETER DRAINS FROM LAST STRESS PERIOD')
      ELSE
         NNPDRN=ITMP
      END IF
!
!3------IF THERE ARE NEW NON-PARAMETER DRAINS, READ THEM.
      MXACTD=IDRNPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPDRN.GT.MXACTD) THEN
            WRITE(IOUT,99) NNPDRN,MXACTD
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE DRAINS (',I6,&
                          ') IS GREATER THAN MXACTD(',I6,')')
            CALL USTOP(' ')
         END IF
         IF(IUNSTR.EQ.0)THEN
            CALL ULSTRD(NNPDRN,DRAI,1,NDRNVL,MXDRN,1,IN,IOUT,&
                'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',&
                DRNAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,5,5,IPRDRN)
         ELSE
            CALL ULSTRDU(NNPDRN,DRAI,1,NDRNVL,MXDRN,1,IN,IOUT,&
                'DRAIN NO.      NODE         DRAIN EL.  CONDUCTANCE',&
                DRNAUX,20,NAUX,IFREFM,NEQS,5,5,IPRDRN)
         ENDIF
      END IF
      NDRAIN=NNPDRN
!
!1C-----IF THERE ARE ACTIVE DRN PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRN')
      IF(NP.GT.0) THEN
         NREAD=NDRNVL-1
         DO 30 N=1,NP
            CALL UPARLSTSUB(IN,'DRN',IOUTU,'DRN',DRAI,NDRNVL,MXDRN,NREAD,&
                MXACTD,NDRAIN,5,5,&
                'DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE',&
                DRNAUX,20,NAUX)
   30    CONTINUE
      END IF
!
!3------PRINT NUMBER OF DRAINS IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NDRAIN
  101 FORMAT(1X,/1X,I6,' DRAINS')
!
!-------FOR STRUCTURED GRID, CALCULATE NODE NUMBER AND PLACE IN LAYER LOCATION
      IF(ITMP.GT.0.AND.IUNSTR.EQ.0)THEN
        DO L=1,NNPDRN  ! ONLY NEEDS TO BE DONE FOR NON-PARAMETER DRAINS
          IR=DRAI(2,L)
          IC=DRAI(3,L)
          IL=DRAI(1,L)
          N = IC + NCOL*(IR-1) + (IL-1)* NROW*NCOL
          DRAI(1,L) = N
        ENDDO
      ENDIF
!
!8------RETURN.
      RETURN
      END SUBROUTINE ReadDRN_StressPeriods
      


    SUBROUTINE UPARARRAL(IN,IOUT,LINE,NP)
!     ******************************************************************
!     Setup array parameter definition for a package.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) LINE
      
      integer :: in, np, iout, lloc, istop
      real :: r
      integer :: n, istart
!     ------------------------------------------------------------------
!
!  If NP has not already been defined, decode PARAMETER definitions if
!  they exist
      IF(IN.GT.0) THEN
         NP=0
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
            READ(IN,'(A)') LINE
         END IF
      END IF
!
!  Process the parameter information
      IF(NP.GT.0) THEN
         WRITE(IOUT,31) NP
   31    FORMAT(1X,I5,' Named Parameters     ')
      ELSE
         NP=0
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
!
      RETURN
    END SUBROUTINE UPARARRAL
    
   SUBROUTINE PRESET(PTYP)
!     ******************************************************************
!     Clear active flag for all parameters of a specified type
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PTYP
      
      integer :: i
!     ------------------------------------------------------------------
!
!1------Loop through all parameters.  Set IACTIVE to 0 when the
!1------parameter type matches.
      DO 10 I=1,IPSUM
      IF(PARTYP(I).EQ.PTYP) IACTIVE(I)=0
   10 CONTINUE
!
!2------Return.
      RETURN
    END SUBROUTINE PRESET

    SUBROUTINE UPARARRSUB2(ZZ,NCOL,NROW,ILAY,NP,IN,IOUT,PTYP,ANAME,&
           PACK,IPF)
!     ******************************************************************
!     Read a series of parameter names and substitute their values into
!     a 2-D array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      DIMENSION ZZ(NCOL,NROW)
      CHARACTER*(*) PTYP,PACK
      CHARACTER*24 ANAME
      CHARACTER*400 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
      
      integer :: in, ncol, nrow, ilay, iout, np, ipf, init, n
      real :: rdum, zz
      integer :: lloc, idum, istart, istop, ip, numinst, iloc,ni, ki, ii, nsub, i
!     ------------------------------------------------------------------
!
!1------Set initialization flag to cause USUB2D to initialze ZZ to 0.
      INIT=1
!
!2------Read each parameter name.
      DO 100 N=1,NP
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
        WRITE(IOUT,5) LINE(ISTART:ISTOP)
    5   FORMAT(' Parameter:  ',A)
        IF(LINE(ISTART:ISTOP).EQ.' ') THEN
          WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
          CALL USTOP(' ')
        END IF
!
!3------Loop through each parameter looking for the specified name.
        CTMP1=LINE(ISTART:ISTOP)
        CALL UPCASE(CTMP1)
        DO 10 IP=1,IPSUM
          CTMP2=PARNAM(IP)
          CALL UPCASE(CTMP2)
          IF(CTMP1.EQ.CTMP2) GO TO 20
!
!3A-----Stop looping if the end of the parameter list is found.
          IF(PARNAM(IP).EQ.' ') GO TO 15
   10   CONTINUE
   15   WRITE(IOUT,16) PACK
   16   FORMAT(1X,'Error in ',A,' file:',/&
           1X,'The above parameter must be defined prior to its use')
        CALL USTOP(' ')
!
!4------Found parameter.
   20   CONTINUE
        IF(PARTYP(IP).NE.PTYP) THEN
!5------Print an error message if the parameter type does not match.
          WRITE(IOUT,83) PARNAM(IP),PARTYP(IP),PACK,PTYP
   83     FORMAT(1X,'Parameter type conflict:',/&
                1X,'Named parameter:',A,' was defined as type:',A,/&
                1X,'However, this parameter is used in the ',A,&
                  ' file, so it should be type:',A)
          CALL USTOP(' ')
        end if
!
!6------Check to see if this parameter is time varying (has instances).
        NUMINST=IPLOC(3,IP)
        ILOC=IPLOC(4,IP)
        NI=1
!
!6A-----If parameter is time-varying, read instance name.
        IF(NUMINST.GT.0) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
          CTMP3=LINE(ISTART:ISTOP)
          IF(CTMP3.EQ.' ') THEN
            WRITE(IOUT,1000) PACK,PARNAM(IP)
 1000       FORMAT(/,1X,'Blank instance name in the ',A,&
                  ' file for parameter ',A)
            CALL USTOP(' ')
          end if
          WRITE(IOUT,1010) CTMP3
 1010     FORMAT(3X,'Instance:  ',A)
          CALL UPCASE(CTMP3)
!
!6B------Look for instance name
          DO 50 KI=1,NUMINST
            CTMP4=INAME(ILOC+KI-1)
            CALL UPCASE(CTMP4)
            IF(CTMP3.EQ.CTMP4) THEN
              NI=KI
              GOTO 55
            end if
   50     CONTINUE
          WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
 1020     FORMAT(/,1X,'The ',A,' file specifies undefined instance "',&
                A,'" for parameter ',A)
          CALL USTOP(' ')
   55     CONTINUE
        end if
!
!7------Check to see if this parameter is already active.
        IF (IACTIVE(IP).GT.0) THEN
          WRITE(IOUT,1030) PARNAM(IP)
 1030     FORMAT(/,1X,'*** ERROR: PARAMETER "',A,&
             '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,&
             ' -- STOP EXECUTION (UPARARRSUB2)')
          CALL USTOP(' ')
        end if
!
!8------Activate the parameter and substitute.  Reset INIT so that
!8------any further calls to USUB2D will not reinitilize ZZ.
        IACTIVE(IP)=NI
        II=IP
        CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,NSUB)
        INIT=0
!
!9------Get new value of print flag if it is there.
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,RDUM,-1,IN)
        IF(LINE(ISTART:ISTOP) .NE.'E' .AND.&
          LINE(ISTART:ISTOP) .NE.' ') IPF=I
!
  100 CONTINUE
!
!10-----PRINT THE ARRAY.
  200 CALL ULAPRWC(ZZ,NCOL,NROW,ILAY,IOUT,IPF,ANAME)
!
!11-----Return.
      RETURN
      END SUBROUTINE UPARARRSUB2

    SUBROUTINE UPARARRRP(IN,IOUT,NP,ILFLG,PTYP,ITERP,ITVP,IACT)
        !     ******************************************************************
        !     Read and store array parameter definition information for one
        !     parameter.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              CHARACTER*(*) PTYP
              CHARACTER*400 LINE
              CHARACTER*10 PN,CTMP1,CTMP2
              
              integer :: in, iact, np, itvp, iterp, ilflg, iout, lloc, n, i, j
              real :: r, pv
              integer :: istop, ni, istart, nclu, numinst, ib, inst, kk
              integer :: im1, im2, iz1, iz2
        !     ------------------------------------------------------------------
        !
        !1------Read a parameter definition line and decode the parameter name,
        !1------type, and value
              READ(IN,'(A)') LINE
              LLOC=1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
              PN=LINE(ISTART:ISTOP)
              CTMP1=PN
              CALL UPCASE(CTMP1)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
              PTYP=LINE(ISTART:ISTOP)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
        !
        !2------Look for the parameter name in the parameter list
              DO 10 NP=1,MXPAR
                CTMP2=PARNAM(NP)
                CALL UPCASE(CTMP2)
                IF(CTMP1.EQ.CTMP2) THEN
        !
        !2A-----If found, determine if it is an illegal duplicate or if it was
        !         predefined.
                  IF(PARTYP(NP).NE.' ' .AND. ITERP.EQ.1) THEN
        !           Illegal duplicate
                    WRITE(IOUT,110) CTMP1
          110       FORMAT(' Duplicate parameter name: ',A)
                    CALL USTOP(' ')
                  END IF
        !         Parameter was predefined -- leave its value alone
        !         (i.e. ignore PV).
                  GO TO 100
                ELSE IF(PARNAM(NP).EQ.' ') THEN
        !         Parameter was not found in the list, so it is a new
        !         definition. Put values in the list.
                  PARNAM(NP)=PN
                  B(NP)=PV
                  IPSUM=IPSUM+1
                  GO TO 100
                END IF
        10    CONTINUE
        !
        !2B-----Entire parameter list has been searched without finding
        !2B-----a blank entry for the new parameter.  Too many parameters
              WRITE(IOUT,11)
           11 FORMAT(1X,'The number of parameters has exceeded the maximum')
              CALL USTOP(' ')
        !
        !3------Parameter is a new parameter or it was prefined in the
        !3------Parameter Value file.  Get the number of clusters.
          100 PARTYP(NP)=PTYP
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLU,R,IOUT,IN)
              IF(NCLU.LE.0) THEN
                WRITE(IOUT,104) PN
          104   FORMAT(' ERROR:  DEFINITION FOR PARAMETER "',A,'"',&
                ' INCLUDES NO CLUSTERS',/,'   -- STOP EXECUTION (UPARARRRP)')
                CALL USTOP(' ')
              end if
              IF(ITERP.EQ.1) THEN
                NUMINST=0
                IF (ITVP.GT.0) THEN
        !
        !4------CHECK FOR MULTIPLE INSTANCES.
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
                  IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
                    IF (NUMINST.LT.1) THEN
                      WRITE(IOUT,12) PARNAM(NP),PTYP
           12         FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',&
                         ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,&
                         '" -- STOP EXECUTION (UPARARRRP)')
                      CALL USTOP(' ')
                    end if
                  end if
                end if
        !
        !5------SET IPLOC VALUES.
                IPLOC(1,NP)=ICLSUM+1
                NI=MAX(1,NUMINST)
                ICLSUM=ICLSUM+NCLU*NI
                IPLOC(2,NP)=ICLSUM
                IPLOC(3,NP)=NUMINST
                IPLOC(4,NP)=INAMLOC
                INAMLOC=INAMLOC+NUMINST
        !
        !6------MAKE SURE THAT THE MAXIMUM NUMBER OF CLUSTERS IN IPCLST IS
        !6------NOT EXCEEDED.
                IF(IPLOC(2,NP).GT.MXCLST) THEN
                  WRITE(IOUT,117) IPLOC(2,NP),MXCLST
          117     FORMAT(1X,I5,&
                ' CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY',I5)
                  WRITE(IOUT,*) NP,NCLU
                  WRITE(IOUT,'(A)') PARNAM(NP)
                  WRITE(IOUT,'(4I10)') IPLOC
                  CALL USTOP(' ')
                END IF
                WRITE(IOUT,121) PARNAM(NP),PARTYP(NP),NCLU
          121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A,'   CLUSTERS:',&
                      I4)
                WRITE(IOUT,122) PV
          122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
                IF(B(NP).NE.PV) THEN
                  WRITE(IOUT,123) B(NP)
          123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,&
                     ', as read from',/,' the Parameter Value file')
                END IF
        !
        !7------MAKE SURE THE MAXIMUM NUMBER OF INSTANCES IS NOT EXCEEDED.
                IF(NUMINST.GT.0) THEN
                  WRITE(IOUT,124)NUMINST
          124      FORMAT(3X,'NUMBER OF INSTANCES: ',I4)
                  IF((INAMLOC-1).GT.MXINST) THEN
                    WRITE(IOUT,125)INAMLOC-1,MXINST
          125       FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/&
                    1X,I5,' instances have been specified'/&
                    1X,'The maximum number of instances is',I5)
                    CALL USTOP(' ')
                  end if
                end if
              ELSE
                NUMINST=IPLOC(3,NP)
              end if
              IACTIVE(NP)=IACT
        !
        !8------Process clusters for each instance.
              IF(NUMINST.EQ.0) THEN
                IB=0
              ELSE
                IB=1
              end if
              I=IPLOC(1,NP)-1
              DO 210 INST=IB,NUMINST
                IF(NUMINST.GT.0) CALL UINSRP(INST,IN,IOUT,NP,ITERP)
        !
        !9------Read and process clusters.
                DO 200 KK=1,NCLU
                  I=I+1
                  READ(IN,'(A)') LINE
                  IF(ITERP.EQ.1) THEN
                    LLOC=1
                    IF(ILFLG.NE.0) THEN
        !
        !9A-----Get layer number for cluster
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(1,I),R,IOUT,&
                                 IN)
                    ELSE
                      IPCLST(1,I)=0
                    END IF
        !
        !9B-----Get multiplier and zone array names.
                    CALL URWORD(LINE,LLOC,IM1,IM2,0,N,R,IOUT,IN)
                    CALL URWORD(LINE,LLOC,IZ1,IZ2,0,N,R,IOUT,IN)
        !
        !9C-----Get zone numbers.
                    DO 30 J=5,14
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(J,I),R,-1,IN)
                      IF(IPCLST(J,I).EQ.0) THEN
                        IPCLST(4,I)=J-1
                        GO TO 32
                      END IF
           30       CONTINUE
                    IPCLST(4,I)=14
           32       CONTINUE
                    IF(ILFLG.NE.0) THEN
                      WRITE(IOUT,36) IPCLST(1,I),LINE(IM1:IM2),LINE(IZ1:IZ2)
           36         FORMAT(16X,'LAYER: ',I3,'    MULTIPLIER ARRAY: ',A,&
                     '    ZONE ARRAY: ',A)
                    ELSE
                      WRITE(IOUT,37) LINE(IM1:IM2),LINE(IZ1:IZ2)
           37         FORMAT(16X,'MULTIPLIER ARRAY: ',A,'    ZONE ARRAY: ',A)
                    END IF
        !
        !9D-----Find the multiplier array number.
                    CTMP1=LINE(IM1:IM2)
                    CALL UPCASE(CTMP1)
                    IF(CTMP1.EQ.'NONE') THEN
                      IPCLST(2,I)=0
                    ELSE
                      IF(NMLTAR.GT.0) THEN
                      DO 40 J=1,NMLTAR
                        CTMP2=MLTNAM(J)
                        CALL UPCASE(CTMP2)
                        IF(CTMP1.EQ.CTMP2) GO TO 45
           40           CONTINUE
                      END IF
                      WRITE(IOUT,'(A)') ' Multiplier array has not been defined'
                      CALL USTOP(' ')
           45         IPCLST(2,I)=J
                    END IF
        !
        !9E-----Find the zone array number.
                    CTMP1=LINE(IZ1:IZ2)
                    CALL UPCASE(CTMP1)
                    IF(CTMP1.EQ.'ALL') THEN
                      IPCLST(3,I)=0
                    ELSE
                      IF(IPCLST(4,I).EQ.4) THEN
                        WRITE(IOUT,47)
           47           FORMAT(1X,&
                       'There were no zone values specified in the cluster',/&
                       1X,'At least one zone must be specified')
                        CALL USTOP(' ')
                      END IF
                      WRITE(IOUT,48) (IPCLST(J,I),J=5,IPCLST(4,I))
           48         FORMAT(1X,'               ZONE VALUES:',10I5)
                      IF(NZONAR.GT.0) THEN
                        DO 50 J=1,NZONAR
                          CTMP2=ZONNAM(J)
                          CALL UPCASE(CTMP2)
                          IF(CTMP1.EQ.CTMP2) GO TO 55
           50           CONTINUE
                      END IF
                      WRITE(IOUT,'(A)') ' Zone array has not been defined'
                      CALL USTOP(' ')
           55         IPCLST(3,I)=J
                    END IF
                  end if
        !
          200   CONTINUE
          210 CONTINUE
        !
        !10-----RETURN.
              RETURN
    END SUBROUTINE UPARARRRP
    
    SUBROUTINE SGWF2LPFU1CK(IOUT,NP,PTYP)
        !     ******************************************************************
        !     CHECK THAT JUST-DEFINED PARAMETER OF TYPE 'VK' OR 'VANI' IS USED
        !     CONSISTENTLY WITH LAYVKA ENTRIES FOR LAYERS LISTED IN CLUSTERS FOR
        !     THE PARAMETER
        !     ******************************************************************
        !
        !      SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GWFBCFMODULE,  ONLY:LAYVKA
              USE PARAMMODULE
        !
              CHARACTER*4 PTYP
              
              integer :: np, iout, icl, lay, lv
        !     ------------------------------------------------------------------
        !
        !1------LOOP THROUGH THE CLUSTERS FOR THIS PARAMETER.
              DO 10 ICL = IPLOC(1,NP),IPLOC(2,NP)
                LAY = IPCLST(1,ICL)
                LV = LAYVKA(LAY)
                IF (PTYP.EQ.'VK  ' .AND. LV.NE.0) THEN
                  WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VK'
          590     FORMAT(/,&
             1X,'LAYVKA entered for layer ',i3,' is: ',i3,'; however,',&
             ' layer ',i3,' is',/,' listed in a cluster for parameter "',a,&
             '" of type ',a,' and')
                  WRITE (IOUT,600)
          600     FORMAT(&
             1X,'parameters of type VK can apply only to layers for which',&
             /,' LAYVKA is specified as zero -- STOP EXECUTION (SGWF2LPFU1CK)')
                  CALL USTOP(' ')
                ELSEIF (PTYP.EQ.'VANI' .AND. LV.EQ.0) THEN
                  WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VANI'
                  WRITE (IOUT,610)
          610     FORMAT(&
             1X,'parameters of type VANI can apply only to layers for which',/,&
             ' LAYVKA is not specified as zero -- STOP EXECUTION',&
             ' (SGWF2LPFU1CK)')
                  CALL USTOP(' ')
                end if
           10 CONTINUE
        !
        !2------Return.
              RETURN
    END SUBROUTINE SGWF2LPFU1CK
   
!-------------------------------------------------------------------------
    SUBROUTINE SGWF2LPFU1S(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,&
               NPVKCB,STOTXT,NOPCHK)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE FOR STRUCTURED GRID
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL, ONLY:NCOL,NROW,NLAY,ITRSS,LAYCBD,ISYM,&
                         IBOUND,BUFF,IOUT,NODES,IVC,&
                         IUNSTR,IA,JA,JAS,NJA,ARAD,IPRCONN
              USE GWFBCFMODULE,ONLY:LAYCON,HK,SC1,SC2,WETDRY,&
                                   laywet,&
                                   LAYTYP,CHANI,LAYVKA,&
                                   LAYFLG,VKA,VKCB,HANI,IHANISO,&
                                   alpha,beta,sr,brook,BP,IBPN,itabrich
        !
              REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
              REAL, DIMENSION (:), ALLOCATABLE :: TEMPPL
        !
              CHARACTER*24 ANAME(15),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /'        WETDRY PARAMETER'/
              DATA ANAME(11) /'                   alpha'/
              DATA ANAME(12) /'                    beta'/
              DATA ANAME(13) /'                      sr'/
              DATA ANAME(14) /'                   brook'/
              DATA ANAME(15) /'     BUBBLING POINT HEAD'/
              REAL PI
              
              integer :: npsy, in, npvani, npss, nphk, nphani, npvk, npvkcb, nopchk, n, i, k, j
              integer :: ii, jj, iis, kk, khani, ianame
        !     ------------------------------------------------------------------
        !1-------ALLOCATE TEMP ARRAY FOR STORING 3-D INFORMATION
              ALLOCATE(TEMP(NCOL,NROW))
              ZERO = 0.0
        !2------SET ANGLE INTO ARAD WHEN THERE IS HORIZONTAL ANISOTROPY
              IF(IHANISO.EQ.1)THEN
                PI = 3.1415926536
        !2A----SET FACE ANGLES IN ARAD
                DO N=1,NODES
                  DO II = IA(N)+1,IA(N+1)-1
                    JJ = JA(II)
                    IF(JJ.GE.N) CYCLE
                    IIS = JAS(II)
                    IF(IVC(IIS).EQ.1) CYCLE
                    IF((N-JJ).EQ.1) THEN
                      ARAD(IIS) = pi
                    ELSEIF((JJ-N).EQ.1) THEN
                      ARAD = 0
                    ELSEIF(JJ .LT. N) THEN
                      ARAD(IIS) =  pi/2.
                    ELSE
                      ARAD(IIS) = -pi/2.
                    end if
                  end do
                end do
        !
        !2B-------WRITE FACE ANGLES ARRAY
                IF(IPRCONN.NE.0)THEN
                  WRITE(IOUT,*)'FACE ANGLE IS BELOW, 22G15.6, UNSYMMETRIC'
                  ALLOCATE(TEMPPL(NJA))
                  DO N=1,NODES
                  DO II = IA(N)+1,IA(N+1)-1
                    JJ = JA(II)
                    IF(JJ.GE.N)THEN
                      IIS = JAS(II)
                      TEMPPL(II) = ARAD(IIS)
                      TEMPPL(ISYM(II)) = ARAD(IIS)
                    end if
                  end do
                  end do
                  WRITE(IOUT,55)(TEMPPL(J),J=1,NJA)
        55      FORMAT(1P,22G15.6)
        !SP          WRITE(IOUT,55)(ARAD(J),J=1,NJAS) !COMMENTED OUT SYMMETRIC WRITE
                  DEALLOCATE (TEMPPL)
                end if
              end if
        !
        !3------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
              DO 200 K=1,NLAY
              KK=K
        !
        !3A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
              IF(NPHK.EQ.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(1),NROW,NCOL,KK,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(1,K)
                 WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
          121    FORMAT(1X,/1X,A,' FOR LAYER',I4,&
                ' will BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
                 CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'HK',&
                   IOUT,ANAME(1),LAYFLG(1,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                 NLAY,NROW,IUNSTR,'HK  ')
              END IF
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                HK(N) = TEMP(J,I)
              end do
              end do
        !
        !3B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
              IF(CHANI(K).LE.ZERO) THEN
                KHANI=-CHANI(K)
                IF(NPHANI.EQ.0) THEN
                   CALL U2DREL(TEMP(1,1),ANAME(2),NROW,NCOL,KK,IN,IOUT)
                ELSE
                   READ(IN,*) LAYFLG(6,K)
                   WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
                   CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'HANI',&
                   IOUT,ANAME(2),LAYFLG(6,KK))
                   IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                   NLAY,NROW,IUNSTR,'HANI')
                END IF
                DO I=1,NROW
                DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  HANI(N) = TEMP(J,I)
                end do
                end do
              END IF
        !
        !3C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
        !3C-----ANISOTROPY (VKA).
              IANAME=3
              PTYP='VK'
              IF(LAYVKA(K).NE.0) THEN
                 IANAME=4
                 PTYP='VANI'
              END IF
              IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(IANAME),NROW,NCOL,KK,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(2,K)
                 WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
                 CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,PTYP,IOUT,&
                                    ANAME(IANAME),LAYFLG(2,KK))
                 IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                  NLAY,NROW,IUNSTR,PTYP)
              END IF
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                VKA(N) = TEMP(J,I)
              end do
              end do
        !
        !3D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
              IF(ITRSS.NE.0) THEN
                 IF(NPSS.EQ.0) THEN
                    CALL U2DREL(TEMP(1,1),STOTXT,NROW,NCOL,KK,IN,IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(3,K)
                    WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
                    CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'SS',&
                        IOUT,STOTXT,LAYFLG(3,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                    NCOL,NLAY,NROW,IUNSTR,'SS  ')
                 END IF
                 DO I=1,NROW
                 DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  SC1(N) = TEMP(J,I)
                 end do
                 end do
              END IF
        !
        !3E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
        !3E-----IS CONVERTIBLE.
              IF(LAYTYP(K).NE.0) THEN
                 IF(ITRSS.NE.0) THEN
                    IF(NPSY.EQ.0) THEN
                       CALL U2DREL(TEMP(1,1),ANAME(7),NROW,NCOL,KK,IN,&
                              IOUT)
                    ELSE
                       READ(IN,*) LAYFLG(4,K)
                       WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
                       CALL UPARARRSUB1(TEMP(1,1),NCOL,&
                      NROW,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
                       IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                       NCOL,NLAY,NROW,IUNSTR,'SY  ')
                    END IF
                    DO I=1,NROW
                    DO J=1,NCOL
                      N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                      SC2(N) = TEMP(J,I)
                    end do
                    end do
                 END IF
              END IF
        !
        !3F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
              IF(LAYCBD(K).NE.0) THEN
                 IF(NPVKCB.EQ.0) THEN
                    CALL U2DREL(TEMP(1,1),ANAME(5),NROW,NCOL,KK,IN,&
                          IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(5,K)
                    WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
                    CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,&
                      'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                    NCOL,NLAY,NROW,IUNSTR,'VKCB')
                 END IF
                 DO I=1,NROW
                 DO J=1,NCOL
                   N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                   VKCB(N) = TEMP(J,I)
                 end do
                 end do
              END IF
        !
        !3G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
        !3G-----(LAYWET NOT 0).
              IF(LAYWET(K).NE.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(8),NROW,NCOL,KK,IN,IOUT)
                 DO I=1,NROW
                 DO J=1,NCOL
                   N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                   WETDRY(N) = TEMP(J,I)
                 end do
                 end do
              END IF
        !4----read parameters for Richards Equation if solving unsaturated zone flow
              IF(LAYCON(K).NE.5)GOTO 300
              IF(ITABRICH.EQ.0) THEN
        !3H-----READ alpha, beta, sr, brook
              CALL U2DREL(TEMP(1,1),ANAME(11),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                alpha(N) = TEMP(J,I)
              end do
              end do
              CALL U2DREL(TEMP(1,1),ANAME(12),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                beta(N) = TEMP(J,I)
              end do
              end do
              CALL U2DREL(TEMP(1,1),ANAME(13),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                sr(N) = TEMP(J,I)
              end do
              end do
              CALL U2DREL(TEMP(1,1),ANAME(14),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                brook(N) = TEMP(J,I)
              end do
              end do
              IF(IBPN.GT.0)THEN
                CALL U2DREL(TEMP(1,1),ANAME(15),NROW,NCOL,KK,IN,IOUT)
                DO I=1,NROW
                DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  bP(N) = TEMP(J,I)
                end do
                end do
              end if
        !6------read tabular input for retention and relative permeability curves
              ELSE


              end if
          300 CONTINUE
        !-------------------------------------------------------------------------
          200 CONTINUE
        !-------------------------------------------------------------------------
              DEALLOCATE(TEMP)
        !
        !4------RETURN
              RETURN
      END SUBROUTINE SGWF2LPFU1S
      
      SUBROUTINE SGWF2LPFU1G(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,&
               NPVKCB,STOTXT,NOPCHK)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE FOR UNSTRUCTURED (GENERAL) GRID
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL,      ONLY:NLAY,ITRSS,LAYCBD,&
                              IBOUND,BUFF,IOUT,ARAD,JAS,&
                              NODES,IUNSTR,PGF,NJAS,TOP,BOT,&
                              NODLAY,IA,JA,IDSYMRD,IATMP,NJATMP,IVC,cl1
              USE GWFBCFMODULE,ONLY:SC1,SC2,WETDRY,&
                                   IKCFLAG,laywet,&
                                   LAYTYP,CHANI,LAYVKA,&
                                   LAYFLG,VKA,VKCB,HANI,HK,IHANISO,&
                                   alpha,beta,sr,brook,BP,IBPN,ITABRICH, LAYCON
        !
              REAL, DIMENSION(:),ALLOCATABLE  ::TEMP
        !
              CHARACTER*24 ANAME(16),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /' CONNECTION CONDUCTIVITY'/
              DATA ANAME(11) /'              FACE ANGLE'/
              DATA ANAME(12) /'                   alpha'/
              DATA ANAME(13) /'                    beta'/
              DATA ANAME(14) /'                      sr'/
              DATA ANAME(15) /'                   brook'/
              DATA ANAME(16) /'     BUBBLING POINT HEAD'/
              
            integer :: npss, nphk, in, nphani, npsy, npvk, npvani, nopchk, npvkcb
            integer :: kk, nndlay, nstrt, khani, ianame, iis, n
            real :: thick1, thick2, thick, akn
            integer :: ndslay, ii, jj, ikn, k

        !     ------------------------------------------------------------------
        !
                ZERO=0.
        !1------READ FACE ANGLES IF ANISOTROPIC
              IF(IHANISO.EQ.1)THEN
                CALL U1DRELNJA(ARAD(1),IATMP,ANAME(11),NJATMP,IN,IOUT,IDSYMRD)
        !---------
        !8A.......CHANGE OUTWARD NORMAL FOR COMPATIBIITY WITH OLDER CROSS-DISPERSION FORMULATION
        ! --------OLDER FORMULATION DID DIFFERENTLY AND Y WAS + UPWARD IN MANY APPLICATIONS
        !        IF(IOUTNORMAL. EQ.0) THEN
        !          DO N=1,NODES
        !            IF(IBOUND(N).EQ.0) CYCLE
        !C3-------GO OVER UPPER CONNECTIONS OF NODE N AND FILL Vx, Vy, Vz IN BOTH
        !            DO II = IA(N)+1,IA(N+1)-1
        !              JJ = JA(II)
        !              IF(JJ.GT.N.AND.JJ.LE.NODES)THEN
        !                IIS = JAS(II)
        !                IF(IBOUND(JJ).NE.0)THEN
        !                   ANGLE = ARAD(IIS)
        !                   IF(ANGLE .GT. 1.57079. AND. ANGLE. LT. 1.57080)THEN
        !                     ARAD(IIS) = -1.570796 ! CONVERT + 90 DEGREES TO - 90 DEGREES
        !                   ELSEIF(ANGLE .GT. 4.712. AND. ANGLE. LT. 4.713)THEN
        !                     ARAD(IIS) = 1.570796  ! CONVERT - 90 DEGREES TO + 90 DEGREES
        !                   end if
        !                end if
        !              end if
        !            end do
        !          end do
        !        end if
              end if
        !2------LOOP OVER ALL LAYERS TO DEFINE ARRAYS
              DO 200 K = 1,NLAY
              KK = K
              NNDLAY = NODLAY(K)
              NSTRT = NODLAY(K-1)+1
              NDSLAY = NNDLAY - NODLAY(K-1)
        !
        !2A-------Perform checks for unstructured grid formulations
              IF(IKCFLAG.NE.0)THEN
                IF(LAYCON(K).EQ.1.OR.LAYCON(K).EQ.3)WRITE(IOUT,10)K
              end if
        10    FORMAT(5X,'**LAYTYP=1 IS NOT ALLOWED WITH IKCFLAG = 1 OR -1,',&
               1X,'SINCE K OF CONNECTIVITY IS READ. CHECK LAYER',I8)
        !-------------------------------------------------
              IF(IKCFLAG.NE.0)GO TO 120
        !-------------------------------------------------
        !3------DEFINE ARRAYS FOR EACH LAYER
        !3A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
              IF(NPHK.EQ.0) THEN
                 CALL U1DREL(HK(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(1,K)
                 WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
          121    FORMAT(1X,/1X,A,' FOR LAYER',I4,&
                ' WILL BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
                 CALL UPARARRSUB1(HK(NSTRT),NDSLAY,1,KK,'HK',&
                   IOUT,ANAME(1),LAYFLG(1,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                 NDSLAY,1,1,IUNSTR,'HK  ')
              END IF
        !
        !3B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
              IF(CHANI(K).LE.ZERO) THEN
                KHANI=-CHANI(K)
                IF(NPHANI.EQ.0) THEN
                   CALL U1DREL(HANI(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
                ELSE
                   READ(IN,*) LAYFLG(6,K)
                   WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
                   CALL UPARARRSUB1(HANI(NSTRT),NDSLAY,1,KK,'HANI',&
                   IOUT,ANAME(2),LAYFLG(6,KK))
                   IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                   NDSLAY,1,1,IUNSTR,'HANI')
                END IF
              END IF
        !
        !3C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
        !3C-----ANISOTROPY (VKA).
              IANAME=3
              PTYP='VK'
              IF(LAYVKA(K).NE.0) THEN
                 IANAME=4
                 PTYP='VANI'
              END IF
              IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
                 CALL U1DREL(VKA(NSTRT),ANAME(IANAME),NDSLAY,K,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(2,K)
                 WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
                 CALL UPARARRSUB1(VKA(NSTRT),NDSLAY,1,KK,PTYP,IOUT,&
                                    ANAME(IANAME),LAYFLG(2,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                 NDSLAY,1,1,IUNSTR,PTYP)
              END IF
        !-------------------------------------------------
        120   CONTINUE
        !-------------------------------------------------
        !
        !3D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
              IF(ITRSS.NE.0) THEN
                 IF(NPSS.EQ.0) THEN
                    CALL U1DREL(SC1(NSTRT),STOTXT,NDSLAY,K,IN,IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(3,K)
                    WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
                    CALL UPARARRSUB1(SC1(NSTRT),NDSLAY,1,KK,'SS',&
                        IOUT,STOTXT,LAYFLG(3,KK))
                    IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                    NDSLAY,1,1,IUNSTR,'SS  ')
                 END IF
              END IF
        !
        !3E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
        !3E-----IS CONVERTIBLE.
              IF(LAYTYP(K).NE.0) THEN
                 IF(ITRSS.NE.0) THEN
                    IF(NPSY.EQ.0) THEN
                       CALL U1DREL(SC2(NSTRT),ANAME(7),NDSLAY,K,IN,IOUT)
                    ELSE
                       READ(IN,*) LAYFLG(4,K)
                       WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
                       CALL UPARARRSUB1(SC2(NSTRT),NDSLAY,&
                      1,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
                     IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                     NDSLAY,1,1,IUNSTR,'SY  ')
                    END IF
                 END IF
              END IF
        !
        !3F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB) IF NODAL INPUT
              IF(IKCFLAG.EQ.0.AND.LAYCBD(K).NE.0) THEN
                 IF(NPVKCB.EQ.0) THEN
                    CALL U1DREL(VKCB(NSTRT),ANAME(5),NDSLAY,K,IN,&
                          IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(5,K)
                    WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
                    CALL UPARARRSUB1(VKCB(NSTRT),NDSLAY,1,KK,&
                      'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                     NDSLAY,1,1,IUNSTR,'VKCB')
                 END IF
              END IF
        !
        !3G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
        !3G-----(LAYWET NOT 0).
              IF(LAYWET(K).NE.0) THEN
                 CALL U1DREL(WETDRY(NSTRT),ANAME(8),NDSLAY,K,IN,IOUT)
              END IF
        !
        !---------------------------------------------------------
              if(LAYCON(k).NE.5) go to 300
              IF(ITABRICH.EQ.0) THEN
        !3H-----READ alpha, beta, brook
              CALL U1DREL(alpha(NSTRT),ANAME(12),NDSLAY,K,IN,IOUT)
              CALL U1DREL(beta(NSTRT),ANAME(13),NDSLAY,K,IN,IOUT)
              CALL U1DREL(sr(NSTRT),ANAME(14),NDSLAY,K,IN,IOUT)
              CALL U1DREL(brook(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
              IF(IBPN.GT.0)THEN
                CALL U1DREL(bP(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
              end if
        !6------read tabular input for retention and relative permeability curves
              ELSE


              end if
          300 CONTINUE
        !---------------------------------------------------------------
          200 CONTINUE
        !---------------------------------------------------------------
              IF(IKCFLAG.NE.0)THEN
        !4--------READ EFFECTIVE SATURATED K OF CONNECTION
                ALLOCATE(TEMP(NJAS))
                CALL U1DRELNJA(TEMP(1),IATMP,ANAME(10),NJATMP,IN,IOUT,IDSYMRD)
                IF(IKCFLAG.EQ.1)THEN
                  DO IIS=1,NJAS
                    PGF(IIS) = PGF(IIS) * TEMP(IIS)
                  end do
        !-----------INCLUDE THICKNESS TERM
                  DO N=1,NODES
                    THICK1 = TOP(N) - BOT(N)
        !-----------GO OVER CONNECTIONS OF NODE N AND FILL FOR UPPER SYMMETRIC PART
                    DO II = IA(N)+1,IA(N+1)-1
                      JJ = JA(II)
                      IF(JJ.GE.N.AND.JJ.LE.NODES)THEN
                        IIS = JAS(II)
        !                IF(IVC(IIS).NE.0) CYCLE ! DO ONLY FOR HORIZONTAL CONNECTION
                        THICK2 = TOP(JJ) - BOT(JJ)
                        THICK = 0.5 * (THICK1 + THICK2)
                        PGF(IIS) = PGF(IIS) * THICK
                      end if
                    end do
                  end do
                ELSE
                  DO IIS=1,NJAS
                    PGF(IIS) = TEMP(IIS)
                  end do
                end if
        !-------SET HK FOR THEIM SOLUTION CONNECTION
                  DO N=1,NODES
                    THICK = TOP(N) - BOT(N)
                    AKN = 0.0
                    IKN = 0
        !-----------GO OVER CONNECTIONS OF NODE N AND FILL FOR UPPER SYMMETRIC PART
                    DO II = IA(N)+1,IA(N+1)-1
                      JJ = JA(II)
                      IF(JJ.LE.NODES)THEN
                        IIS = JAS(II)
                        IF(IVC(IIS).EQ.0)THEN
                          IF(IKCFLAG.EQ.1) THEN
                           AKN = AKN + PGF(IIS) / THICK
                          ELSE
                            AKN = AKN + PGF(IIS) / THICK * CL1(IIS)
                          end if
                        end if
                        IKN = IKN + 1
                      end if
                    end do
                    IF(IKN.GT.0) THEN
                      HK(N) = AKN / IKN
                    end if
                  end do
                DEALLOCATE(TEMP)
              end if
        !
        !5------RETURN
              RETURN
    END SUBROUTINE SGWF2LPFU1G

    SUBROUTINE UPARARRSUB1(ZZ,NCOL,NROW,ILAY,PTYP,IOUT,ANAME,IPF)
        !     ******************************************************************
        !     Substitute parameter-based values into a 2-D array based on a
        !     parameter type.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              DIMENSION ZZ(NCOL,NROW)
              CHARACTER*(*) PTYP
              CHARACTER*24 ANAME
              
              integer :: ncol, ilay, nrow, iout, ipf, init, ip, ii, nsub
              real :: zz
        !     ------------------------------------------------------------------
        !
        !1------Set initialization flag to cause USUB2D to initialze ZZ to 0.
        !1------Write a header above the list of parameters that define ZZ.
              INIT=1
              WRITE(IOUT,11) ANAME
           11 FORMAT(1X,/,1X,A,' is defined by the following parameters:')
        !
        !2------Loop through each parameter looking for the specified file type.
              DO 100 IP=1,IPSUM
        !
        !2A-----Stop looping if the end of the parameter list is found.
              IF(PARNAM(IP).EQ.' ') GO TO 200
        !
        !2B-----Check for the specified parameter type.
              IF(PARTYP(IP).EQ.PTYP) THEN
        !
        !2C-----Loop through each cluster definition for layers that match the
        !2C-----specified layer.
                 II=IP
                 CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,NSUB)
                 INIT=0
                 IF(NSUB.GT.0) WRITE(IOUT,47) PARNAM(IP)
           47    FORMAT(1X,A)
              END IF
          100 CONTINUE
        !
        !3------PRINT THE ARRAY.
          200 CALL ULAPRWC(ZZ,NCOL,NROW,ILAY,IOUT,IPF,ANAME)
        !
        !4------Return.
              RETURN
    END SUBROUTINE UPARARRSUB1

    SUBROUTINE UPARARRCK(BUFF,IBOUND,IOUT,LAY,NCOL,NLAY,NROW,&
       IUNSTR,PTYP)
!     ******************************************************************
!     CHECK FOR COMPLETE DEFINITION OF ONE LAYER OF CELLS BY ARRAY
!     PARAMETERS OF A GIVEN TYPE.
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      INTEGER IBOUND(NCOL,NROW,NLAY)
      REAL BUFF(NCOL,NROW)
      CHARACTER*4 PTYP
      
      integer :: lay, iout, iunstr, i, j, ip, ic, iza, ii
      integer :: nlay, ncol, nrow, izi, iz, ilay, ierr, kk, ij, jj
!     ------------------------------------------------------------------
!
!1------Make sure that the parameter type is non-blank.
      IF (PTYP.EQ.' ') THEN
        WRITE (IOUT,500)
  500   FORMAT(1X,'ERROR: BLANK PARAMETER TYPE -- STOP EXECUTION',&
            ' (UPARARRCK)')
        CALL USTOP(' ')
      end if
!
!2------Initialize BUFF to 0.
      DO 20 I = 1, NROW
        DO 10 J = 1, NCOL
          BUFF(J,I) = 0.0
   10   CONTINUE
   20 CONTINUE
!
!3------Loop through parameters to find matching parameter type.
!3------Increment BUFF for each cell where a parameter of the specified
!3------type applies.
      DO 100 IP = 1, IPSUM
        IF (PARTYP(IP).EQ.PTYP) THEN
!
!3A-----Loop through clusters associated with this parameter.
          DO 80 IC = IPLOC(1,IP), IPLOC(2,IP)
            IF (IPCLST(1,IC) .EQ. LAY) THEN
              IZA = IPCLST(3,IC)
              DO 60 I = 1, NROW
                DO 50 J = 1,NCOL
                  IF (IZA.GT.0) THEN
!
!3B-----Loop through zones listed for this cluster.
                    DO 40 IZI = 5, IPCLST(4,IC)
                      IZ = IPCLST(IZI,IC)
                      IF (IZ.EQ.IZON(J,I,IZA)) THEN
                        BUFF(J,I) = BUFF(J,I) + 1.0
                      end if
   40               CONTINUE
                  ELSE
!
!3C-----Zones do not apply to this cluster -- apply to all cells.
                    BUFF(J,I) = BUFF(J,I) + 1.0
                  end if
   50           CONTINUE
   60         CONTINUE
            end if
   80     CONTINUE
        end if
  100 CONTINUE
!
!4------Identify any active cells where BUFF is equal to zero, which
!4------indicates cells that are not defined by any parameter of the
!4------specified type applies.
      ILAY = LAY
      IF(IUNSTR.EQ.1) ILAY = 1
      IERR = 0
      DO 140 I = 1, NROW
        DO 120 J = 1, NCOL
          IF (IBOUND(J,I,ILAY).NE.0) THEN
            IF (BUFF(J,I).EQ.0.0)THEN
              IF(IUNSTR.NE.0)THEN
                KK = LAY
                IJ = J - (KK-1)*NCOL*NROW
                II = (IJ-1)/NCOL + 1
                JJ = IJ - (II-1)*NCOL
                WRITE (IOUT,510) II,JJ,KK,PTYP
  510           FORMAT(1X,'ROW: ',I5,', COLUMN: ',I5,' IN LAYER ',I3,&
             ' NOT DEFINED FOR PARAMETER TYPE ',A)
              ELSE
                WRITE (IOUT,511) J,LAY,PTYP
  511           FORMAT(1X,'NODE: ',I9,', IN LAYER: ',I6,&
             ' NOT DEFINED FOR PARAMETER TYPE ',A)
              end if
              IERR = IERR + 1
            end if
          end if
  120   CONTINUE
  140 CONTINUE
!
!5------IF any active cells were found with undefined values, write an
!5------error message and stop.
      IF (IERR.GT.0) THEN
        WRITE (IOUT,520)
  520   FORMAT(/,1X,'PARAMETER DEFINITIONS INCOMPLETE -- STOP',&
            ' EXECUTION (UPARARRCK)')
        CALL USTOP(' ')
      end if
!
!6------Return.
      RETURN
      END SUBROUTINE UPARARRCK
      
      SUBROUTINE U1DRELNJA(ARRAY,IAG,ANAME,NJAG,IN,IOUT,IDSYMRD)
!     ******************************************************************
!     READ SYMMETRIC SUBSURFACE PROPERTY ARRAY FOR UNSTRUCTURED GRIDS
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IA,JA,JAS,NJAS,NODES,NJA
      CHARACTER*24 ANAME
      REAL, DIMENSION(:),ALLOCATABLE  ::TEMP,TEMPU
      DIMENSION ARRAY(NJAS)
      INTEGER IAG(NODES+1)
      
      integer :: iout, idsymrd, njag
      real :: array
      integer :: in, k, njags, n, iic, ii, jj, iis, iagnum, iig
      
!     ------------------------------------------------------------------
      K = 0
      IF(IDSYMRD.EQ.1)THEN
!1------READ SYMMETRIC DATA
        IF(NJA.EQ.NJAG)THEN  !
!1A-------NO CLN OR GNC NODES SO FILL ARRAY DIRECTLY AND RETURN
          CALL U1DREL(ARRAY,ANAME,NJAS,K,IN,IOUT)
          RETURN
        end if
!
        NJAGS = (NJAG - NODES) / 2
        ALLOCATE(TEMP(NJAGS))
!1-------READ SYMMETRIC DATA IN TEMP LOCATION FOR SUBSURFACE NODES
        CALL U1DREL(TEMP,ANAME,NJAGS,K,IN,IOUT)
!1A------FILL INTO UNSYMMETRIC TEMPU LOCATION
        DO N=1,NODES
          IIC = 0  ! ITERATION COUNTER OF GROUNDWATER CONNECTIONS
          DO II = IA(N)+1,IA(N+1)-1
            JJ = JA(II)
            IF(JJ.GT.N .AND. JJ.LE. NODES)THEN
              IIC = IIC + 1
              IIS = JAS(II)
              ARRAY(IIS) = TEMP(IIC)
            end if
          end do
        end do
        DEALLOCATE(TEMP)
        RETURN
      ELSE
!2------READ UNSYMMETRIC DATA IN TEMPU LOCATION AND TRANSFER
        ALLOCATE(TEMPU(NJAG))
        CALL U1DREL(TEMPU(1),ANAME,NJAG,K,IN,IOUT)
      end if
!3------COPY ONLY UPPER TRIANGLE OF TEMPU INTO SYMMETRIC ARRAY FOR SUBSURFACE NODES
      DO N=1,NODES
        IAGNUM = IAG(N+1)-IAG(N)
        IIC = 0 ! ITERATION COUNTER OF II LOOP
!
         DO II = IA(N), IA(N)+IAGNUM-1
          IIC = IIC + 1
          IIG = IAG(N)+IIC-1
          JJ = JA(II)
          IF(JJ.LE.N) CYCLE
          IIS = JAS(II)
          ARRAY(IIS) = TEMPU(IIG)
        end do
      end do
      DEALLOCATE(TEMPU)
!
!4------RETURN
      RETURN
      END SUBROUTINE U1DRELNJA
      
      SUBROUTINE USUB2D(ZZ,NCOL,NROW,IP,ILAY,INIT,NSUB)
!     ******************************************************************
!     Substitute values for a single parameter into a 2-D array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      real :: ZZ(NCOL,NROW)
      
      integer :: init, nsub, ncol, ilay, ip, nrow, i,j, icstart, icstop
      real :: aa
      integer :: numinst, nclu, ni, ic, mlt, iz, jj
      
!     ------------------------------------------------------------------
!
!1------Define constants.
      ZERO=0.0
!
!2------Initialize the array if INIT is not 0.
      IF(INIT.NE.0) THEN
        DO 10 I=1,NROW
          DO 5 J=1,NCOL
            ZZ(J,I)=ZERO
    5     CONTINUE
   10   CONTINUE
      END IF
!
!3------Identify clusters, which depends on the instance if the
!3------parameter is a time varying parameter.
      ICSTART=IPLOC(1,IP)
      ICSTOP=IPLOC(2,IP)
      NUMINST=IPLOC(3,IP)
      IF(NUMINST.GT.1) THEN
!       Select correct instance
        NCLU=(ICSTOP-ICSTART+1)/NUMINST
        NI=IACTIVE(IP)
        ICSTART=ICSTART+(NI-1)*NCLU
        ICSTOP=ICSTART+NCLU-1
      end if
!
!4------Loop through each cluster definition for layers that match the
!4------specified layer.
      NSUB=0
      DO 80 IC=ICSTART,ICSTOP
!
!4A-----Check if the cluster layer matches the specified layer
        IF(IPCLST(1,IC).EQ.ILAY) THEN
!
!4B-----The parameter layer matches the specified layer.  Look at zone
!4B-----value to determine which cells to substitute. Also identify the
!4B-----multiplier array.
          MLT=IPCLST(2,IC)
          AA=1.
          IZ=IPCLST(3,IC)
          IF(IZ.GT.0) THEN
!
!4C-----IZ>0. Loop through all cells.  If the value in the zone array
!4C-----is equal to one of the cluster zone values, add the parameter
!4C-----value into the array.
            DO 50 I=1,NROW
              DO 40 J=1,NCOL
                DO 30 JJ=5,IPCLST(4,IC)
                  IF(IZON(J,I,IZ).EQ.IPCLST(JJ,IC)) THEN
                    IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                    ZZ(J,I)=ZZ(J,I)+AA*B(IP)
                    NSUB=NSUB+1
                  END IF
   30           CONTINUE
   40         CONTINUE
   50       CONTINUE
          ELSE
!
!4D-----IZ is 0.  Loop through all cells adding the parameter value into
!4D-----the array.
            DO 70 I=1,NROW
              DO 60 J=1,NCOL
                IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                ZZ(J,I)=ZZ(J,I)+AA*B(IP)
   60         CONTINUE
   70       CONTINUE
            NSUB=NSUB+NCOL*NROW
          END IF
        END IF
   80 CONTINUE
!
!5------Return.
      RETURN
      END SUBROUTINE USUB2D
      
      SUBROUTINE ULAPRWC(A,NCOL,NROW,ILAY,IOUT,IPRN,ANAME)
!     ******************************************************************
!     WRITE A TWO-DIMENSIONAL REAL ARRAY.  IF THE ARRAY IS CONSTANT,
!     PRINT JUST THE CONSTANT VALUE.  IF THE ARRAY IS NOT CONSTANT, CALL
!     ULAPRW TO PRINT IT.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      real :: A(NCOL,NROW)
      CHARACTER*(*) ANAME
      
      integer :: nrow, iout, iprn, ilay, ncol, i,j
      real :: tmp
      
!     ------------------------------------------------------------------
!
!  Check to see if entire array is a constant.
      TMP=A(1,1)
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
      IF(A(J,I).NE.TMP) GO TO 400
  300 CONTINUE
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,302) ANAME,TMP,ILAY
  302    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
         WRITE(IOUT,303) ANAME,TMP
  303    FORMAT(1X,/1X,A,' =',1P,G14.6)
      ELSE
         WRITE(IOUT,304) ANAME,TMP
  304    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR CROSS SECTION')
      END IF
      RETURN
!
!  Print the array.
  400 IF(ILAY.GT.0) THEN
         WRITE(IOUT,494) ANAME,ILAY
  494    FORMAT(1X,//11X,A,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
         WRITE(IOUT,495) ANAME
  495    FORMAT(1X,//11X,A)
      ELSE
         WRITE(IOUT,496) ANAME
  496    FORMAT(1X,//11X,A,' FOR CROSS SECTION')
      END IF
      IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,NCOL,NROW,0,IPRN,IOUT)
!
      RETURN
      END SUBROUTINE ULAPRWC
      
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
!     ******************************************************************
!     PRINT 1 LAYER ARRAY
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      real :: BUF(NCOL,NROW)
      
      integer :: kper, ilay, iout, iprn, kstp, ip, i, j, ncol, nrow
!     ------------------------------------------------------------------
!
!1------PRINT A HEADER DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I4,&
          ' IN STRESS PERIOD ',I4/2X,75('-'))
      ELSE IF(ILAY.LT.0) THEN
         WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I4,&
          ' IN STRESS PERIOD ',I4/1X,79('-'))
      END IF
!
!2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
!2------BETWEEN 1 AND 21.
    5 IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.21) IP=12
!
!3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)
      IF(IP.GE.3 .AND. IP.LE.6) CALL UCOLNO(1,NCOL,3,15,8,IOUT)
      IF(IP.GE.7 .AND. IP.LE.11) CALL UCOLNO(1,NCOL,3,20,6,IOUT)
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)
      IF(IP.GE.13 .AND. IP.LE.18) CALL UCOLNO(1,NCOL,3,10,7,IOUT)
      IF(IP.EQ.19) CALL UCOLNO(1,NCOL,0,5,13,IOUT)
      IF(IP.EQ.20) CALL UCOLNO(1,NCOL,0,6,12,IOUT)
      IF(IP.EQ.21) CALL UCOLNO(1,NCOL,0,7,10,IOUT)
!
!4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,&
           180,190,200,210), IP
!
!------------ FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
   11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
!
!------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
   21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
!
!------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
   31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
!
!------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
   41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
!
!------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
   51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
!
!------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
   61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
!
!------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
   71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
!
!------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
   81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
!
!------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
   91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
!
!------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
  101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
!
!------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
  111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
!
!------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
  121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
!
!------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
  131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
!
!------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
  141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
!
!------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
  151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
!
!------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
  161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
!
!------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
  171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
!
!------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
  181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
      GO TO 1000
!
!------------FORMAT 5G12.5
  190 WRITE(IOUT,191) I,(BUF(J,I),J=1,NCOL)
  191 FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
      GO TO 1000
!
!------------FORMAT 6G11.4
  200 WRITE(IOUT,201) I,(BUF(J,I),J=1,NCOL)
  201 FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
      GO TO 1000
!
!------------FORMAT 7G9.2
  210 WRITE(IOUT,211) I,(BUF(J,I),J=1,NCOL)
  211 FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
!
 1000 CONTINUE
!
!5------RETURN
      RETURN
      END SUBROUTINE ULAPRW

      SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)
!     ******************************************************************
!     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
!        NLBL1 IS THE START COLUMN LABEL (NUMBER)
!        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
!        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
!        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
!        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
!        IOUT IS OUTPUT CHANNEL
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*1 DOT,SPACE,DG,BF
      DIMENSION BF(130),DG(10)
!
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/&
              '0','1','2','3','4','5','6','7','8','9'/
      DATA DOT,SPACE/'.',' '/
      
      integer :: ndig, iout, nspace, ncpl, nlbl1, nlbl, n, ntot, j1, j2
      integer nlbl2, nwrap, i, nbf, j, i1, i2, i3, i4 
!     ------------------------------------------------------------------
!
!1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
!1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL=NLBL2-NLBL1+1
      N=NLBL
      IF(NLBL.GT.NCPL) N=NCPL
      NTOT=NSPACE+N*NDIG
      IF(NTOT.GT.130) GO TO 50
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
!
!2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
!
!3------CLEAR THE BUFFER (BF).
      DO 20 I=1,130
      BF(I)=SPACE
   20 CONTINUE
      NBF=NSPACE
!
!4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
!
!5------LOAD THE COLUMN #'S INTO THE BUFFER.
      DO 30 J=J1,J2
      NBF=NBF+NDIG
      I2=J/10
      I1=J-I2*10+1
      BF(NBF)=DG(I1)
      IF(I2.EQ.0) GO TO 30
      I3=I2/10
      I2=I2-I3*10+1
      BF(NBF-1)=DG(I2)
      IF(I3.EQ.0) GO TO 30
      I4=I3/10
      I3=I3-I4*10+1
      BF(NBF-2)=DG(I3)
      IF(I4.EQ.0) GO TO 30
      IF(I4.GT.9) THEN
!5A-----If more than 4 digits, use "X" for 4th digit.
         BF(NBF-3)='X'
      ELSE
         BF(NBF-3)=DG(I4+1)
      END IF
   30 CONTINUE
!
!6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
      WRITE(IOUT,31) (BF(I),I=1,NBF)
   31 FORMAT(1X,130A1)
!
   40 CONTINUE
!
!7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
   50 NTOT=NTOT
      IF(NTOT.GT.130) NTOT=130
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,130A1)
!
!8------RETURN
      RETURN
      END SUBROUTINE UCOLNO
      
      SUBROUTINE UPARLSTAL(IN,IOUT,LINE,NP,MXL)
!     ******************************************************************
!     Setup list parameter definition for a package
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) LINE
      
      integer ::  mxl, in, np, iout, lloc, istart
      real :: r
      integer :: n, istop
!     ------------------------------------------------------------------
!
!1------Decode PARAMETER definitions if they exist
      NP=0
      MXL=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
         IF(NP.LT.0) NP=0
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
         IF(MXL.LT.0) MXL=0
         WRITE(IOUT,31) NP,MXL
   31    FORMAT(1X,I10,' Named Parameters     ',I10,' List entries')
         READ(IN,'(A)') LINE
      ELSE
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
!
!2------Return.
      RETURN
      END SUBROUTINE UPARLSTAL
      
      SUBROUTINE UPARLSTSUB(IN,PACK,IOUTU,PTYP,RLIST,LSTVL,LSTDIM,NREAD,&
                     MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX)
!     ******************************************************************
!     Read a list parameter name, look it up in the list of parameters,
!     and substitute values into active part of package array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GLOBAL, ONLY:NCOL,NROW,IUNSTR
      CHARACTER*(*) PACK,PTYP
      DIMENSION RLIST(LSTVL,LSTDIM)
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      CHARACTER*400 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
      
      integer ::  in
      real :: rlist
      integer :: nread, ioutu, ipvl1, naux, mxlst, ipvl2, ntot
      integer :: lstvl, ncaux, lloc, idum, istop, istart
      real :: rdum
      integer :: ip, nlst, numinst, iloc, ni, ki, i, ii, iii
      integer :: j, ipvl, il, ir, ic, jj, n, lstdim
      
!     ------------------------------------------------------------------
!
!1------The LIST file unit is the absolute value of IOUTU.
!1------Read the parameter name.
      IOUT = ABS(IOUTU)
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,1) LINE(ISTART:ISTOP)
    1 FORMAT(/,' Parameter:  ',A)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        CALL USTOP(' ')
      END IF
!
!2------Find the parameter in the list of parameters.
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,IPSUM
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,11) PARNAM(IP),PARTYP(IP),PACK,PTYP
   11       FORMAT(1X,'Parameter type conflict:',/&
                1X,'Named parameter:',A,' was defined as type:',A,/&
                1X,'However, this parameter is used in the ',A,&
                ' file, so it should be type:',A)
            CALL USTOP(' ')
          END IF
!
!3------Set indices to point to the cells that correspond to the
!3------specified parameter.  If the parameter is time varying, set the
!3------indices to the specified instance.
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST.GT.0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              WRITE(IOUT,15)PACK,PARNAM(IP)
   15         FORMAT(/,1X,'Blank instance name in the ',A,&
                    ' file for parameter ',A)
              CALL USTOP(' ')
            ENDIF
            WRITE(IOUT,17) CTMP3
   17       FORMAT(3X,'Instance:  ',A)
            CALL UPCASE(CTMP3)
            DO 50 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 55
              ENDIF
   50       CONTINUE
            WRITE(IOUT,53) PACK,CTMP3,PARNAM(IP)
   53       FORMAT(/,1X,'The ',A,' file specifies undefined instance "',&
                  A,'" for parameter ',A)
            CALL USTOP(' ')
   55       CONTINUE
          ENDIF
!
!4------Check that the parameter is not already active.
          IF (IACTIVE(IP).GT.0) THEN
            WRITE(IOUT,73) PARNAM(IP)
   73       FORMAT(/,1X,'*** ERROR: PARAMETER "',A,&
               '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,&
               ' -- STOP EXECUTION (UPARLSTSUB)')
            CALL USTOP(' ')
          ENDIF
!
!5------Set the active flag.
          IACTIVE(IP)=NI
!
!6------Accumulate the total number of active cells in the list.
          NTOT=NTOT+NLST
          IF(NTOT.GT.MXLST) THEN
            WRITE(IOUT,83) NTOT,MXLST
   83       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,&
               ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
            CALL USTOP(' ')
          END IF
!
!7------Write label for list values if IOUTU is positive.
          IF (IOUTU.GT.0) CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
!
!8------Copy the values from the paramter location into the front part
!8------of the list where the currently active list is kept.
          DO 90 I=1,NLST
            II=NTOT-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            DO 85 J=1,NREAD
              RLIST(J,II)=RLIST(J,III)
   85       CONTINUE
!
!8A-----Scale the RLIST values from IPVL1 to IPVL2 by the parameter
!8A-----value.
            DO 86 IPVL=IPVL1,IPVL2
              RLIST(IPVL,II)=RLIST(IPVL,II)*B(IP)
   86       CONTINUE
            IL=RLIST(1,II)
            IR=RLIST(2,II)
            IC=RLIST(3,II)
            IF (IOUTU.GT.0) WRITE(IOUT,89) II,IL,IR,IC,&
               (RLIST(JJ,II),JJ=4,NREAD)
   89       FORMAT(1X,I6,I7,I7,I7,14G16.4)
!
!DL 1/9/7 PUT NODENUMBER IN COLUMNN 1
            IF(IUNSTR.EQ.0) THEN
              N = IC + NCOL*(IR-1) + (IL-1)* NROW*NCOL
              RLIST(1,II)=N
            ENDIF
   90     CONTINUE
!
!8B------After moving the data, return.
          RETURN
        END IF
  100 CONTINUE
!
!9------All parameter names have been checked without finding the
!9------parameter. Write an error message and stop.
      WRITE(IOUT,*) ' The ',PACK, &
        ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      CALL USTOP(' ')
!
      END SUBROUTINE UPARLSTSUB


        
    subroutine ReadBinary_HDS_File(Modflow, domain)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        integer :: k, nndlay, nstrt, ndslay, ilay
        
        type (ModflowProject) Modflow
        type(ModflowDomain) domain

        allocate(domain.head(domain.nCells,Modflow.ntime))
        
        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading '//trim(domain.name)//' head data from unit ',domain.iHDS,' file '//trim(domain.FNameHDS)
        call msg(TmpSTR)
        if(domain.name=='GWF') then
            call msg('    Period      Step     Layer      Time Cell1Head            Name')
            do i=1,Modflow.ntime
                DO K=1,NLAY
                    NNDLAY = NODLAY(K)
                    NSTRT = NODLAY(K-1)+1
                    NDSLAY = NNDLAY - NODLAY(K-1)          
                    CALL ULASAVURD(domain.head(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                        TOTIMREAD,NSTRT,NNDLAY,ILAY,domain.iHDS,NODES)
                    WRITE(TmpSTR,'(3(i10), 2(1pg10.2), a)') KPERREAD,KSTPREAD,K,TOTIMREAD,domain.head(1,i),TEXT
                    call msg(TmpSTR)
                END DO
            end do
        else
            call msg('    Period      Step      Time Cell1Head            Name')
            do i=1,Modflow.ntime
                IDUM = 1
                CALL ULASAVRD(domain.head(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                    TOTIMREAD,domain.ncells,IDUM,IDUM,domain.iHDS)
          
!                CALL ULASAV(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
!1         1,1,ICLNHD)

          WRITE(TmpSTR,'(2(i10), 2(1pg10.2), a)') KPERREAD,KSTPREAD,TOTIMREAD,domain.head(1,i),TEXT
                call msg(TmpSTR)
            end do
        end if

    end subroutine ReadBinary_HDS_File
    
    subroutine ReadBinary_DDN_File(Modflow, domain)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        integer :: k, nndlay, nstrt, ndslay, ilay
        
        type (ModflowProject) Modflow
        type(ModflowDomain) domain

        allocate(domain.Drawdown(domain.nCells,Modflow.ntime))
        
        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading '//trim(domain.name)//' saturation data from unit ',domain.iDDN,' file '//trim(domain.FNameDDN)
        call msg(TmpSTR)
        if(domain.name=='GWF') then
            call msg('    Period      Step     Layer      Time  Cell1Sat           Name')
            do i=1,Modflow.ntime
                DO K=1,NLAY
                    NNDLAY = NODLAY(K)
                    NSTRT = NODLAY(K-1)+1
                    NDSLAY = NNDLAY - NODLAY(K-1)          
                    CALL ULASAVURD(domain.Drawdown(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                        TOTIMREAD,NSTRT,NNDLAY,ILAY,domain.iDDN,NODES)
                    WRITE(TmpSTR,'(3(i10), 2(1pg10.2), a)') KPERREAD,KSTPREAD,K,TOTIMREAD,domain.Drawdown(1,i),TEXT
                    call msg(TmpSTR)
                END DO
            end do
        else
            call msg('    Period      Step      Time  Cell1Sat            Name')
            do i=1,Modflow.ntime
                IDUM = 1
                CALL ULASAVRD(domain.Drawdown(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                    TOTIMREAD,domain.ncells,IDUM,IDUM,domain.iDDN)
                WRITE(TmpSTR,'(2(i10), 2(1pg10.2), a)') KPERREAD,KSTPREAD,TOTIMREAD,domain.Drawdown(1,i),TEXT
                call msg(TmpSTR)
            end do
        end if

    end subroutine ReadBinary_DDN_File

    subroutine ReadBinary_CBB_File(Modflow, domain)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer :: kstp,kper,NVAL,idum,ICODE
        character*16   :: text
        character*16  :: CompName(100)
        real, allocatable :: dummy(:)
        
        
        !real :: rmin
        !real :: rmax
        integer :: i, j, k

        
        type (ModflowProject) Modflow
        type (ModflowDomain) domain


        FNum=domain.iCBB
        
        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading '//trim(domain.name)//' cell-by-cell flow data from unit ',domain.iCBB,' file '//trim(domain.FNameCBB)
        call msg(TmpSTR)
        
        ! Count cbb components
        call msg('   Component       Name')
        domain.nComp=0
        Component2: do 
                read(FNum,iostat=status) KSTP,KPER,TEXT,NVAL,idum,ICODE
                
                if(status/=0) then
                    exit
                end if
              
                if((NVAL.le.0)) cycle
                if(ICODE .gt. 0)then
                    allocate(dummy(NVAL))
                    read(FNum) (dummy(I),I=1,NVAL)
                    deallocate(dummy)

                    if(CompName(1)==text) exit
                    domain.nComp=domain.nComp+1
                    CompName(domain.nComp)=text
                    write(TmpSTR,*) domain.nComp,CompName(domain.nComp)
                    call Msg(TmpSTR)

                    cycle
                end if
            end do component2    
        
        continue
        
        rewind(FNum)
        call msg('    Period      Step      NVAL     ICODE            Name')
        do j=1,modflow.ntime
            do k=1,domain.nComp
                read(FNum,err=9300,end=1000) KSTP,KPER,TEXT,NVAL,idum,ICODE
                WRITE(TmpSTR,'(4(i10),(a))') KPER,KSTP,NVAL,ICODE,TEXT
                call Msg(TmpSTR)


              
                if((NVAL.le.0)) go to 9300
                if(ICODE .gt. 0)then
                    if(index(TEXT,'FLOW JA FACE') .ne. 0 .or. &
                       index(TEXT,'FLOW CLN FACE') .ne. 0 .or. &
                       index(TEXT,'FLOW SWF FACE') .ne. 0) then
                        if(j==1) then
                            allocate(domain.Cbb_FLOW_FACE(NVAL,Modflow.ntime))
                            domain.Cbb_FLOW_FACE=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.Cbb_FLOW_FACE(I,j),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,domain.Cbb_FLOW_FACE(I,ntime))
                        !    rmin=min(rmin,domain.Cbb_FLOW_FACE(I,ntime))
                        !end do
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'STORAGE') .ne.0) then
                        if(j==1) THEN
                            allocate(domain.cbb_STORAGE(NVAL,Modflow.ntime))
                            domain.cbb_STORAGE=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_STORAGE(I,j),I=1,NVAL)

                    else if(index(text,'CONSTANT HEAD') .ne.0 .or. &
                            index(text,'CLN CONST HEAD') .ne.0 .or. &
                            index(text,'SWF CONST HEAD') .ne.0) then
                        if(j==1) THEN
	                        allocate(domain.cbb_CONSTANT_HEAD(NVAL,Modflow.ntime))
                            domain.cbb_CONSTANT_HEAD=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_CONSTANT_HEAD(I,j),I=1,NVAL)
                            
                    else if(index(text,'RECHARGE') .ne.0) then
                        if(j==1) THEN
	                        allocate(domain.cbb_RECHARGE(NVAL,Modflow.ntime))
                            domain.cbb_RECHARGE=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_RECHARGE(I,j),I=1,NVAL)
                            
                    else if(index(text,'WELLS') .ne.0) then
                        if(j==1) THEN
	                        allocate(domain.cbb_WELLS(NVAL,Modflow.ntime))
                            domain.cbb_WELLS=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_WELLS(I,j),I=1,NVAL)
                            
                    else if(index(text,'DRAINS') .ne.0) then
                        if(j==1) THEN
	                            allocate(domain.cbb_DRAINS(NVAL,Modflow.ntime))
                                domain.cbb_DRAINS=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_DRAINS(I,j),I=1,NVAL)
                            
                    else if(index(text,'CLN') .ne.0) then
                        if(j==1) THEN
	                            allocate(domain.cbb_CLN(NVAL,Modflow.ntime))
                                domain.cbb_CLN=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_CLN(I,j),I=1,NVAL)
                            
                    else if(index(text,'SWF') .ne.0) then
                        if(j==1) THEN
	                            allocate(domain.cbb_SWF(NVAL,Modflow.ntime))
                                domain.cbb_SWF=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_SWF(I,j),I=1,NVAL)
                    else if(index(text,'GWF') .ne.0) then
                        if(j==1) THEN
	                            allocate(domain.cbb_GWF(NVAL,Modflow.ntime))
                                domain.cbb_GWF=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_GWF(I,j),I=1,NVAL)

                    else if(index(text,'SWBC') .ne.0) then
                        if(j==1) THEN
	                            allocate(domain.cbb_SWBC(NVAL,Modflow.ntime))
                                domain.cbb_SWBC=0
                        end if
                        read(FNum,err=9400,end=9400) (domain.cbb_SWBC(I,j),I=1,NVAL)

                    else 
                        call ErrMsg(trim(domain.FNameCBB)//': TEXT variable '//text//' not currently recognized ')
                    end if
                else if(ICODE .eq. -1) then
                    call Msg( 'The budget data is written in the compact budget style.')
                    call Msg( 'Not supported yet.')
                    stop
                end if
            end do 



        end do
9300    continue
9400    continue
1000    continue
 
    end subroutine ReadBinary_CBB_File

    SUBROUTINE ULASAVRD(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,&
                        NROW,ILAY,ICHN)
!     ******************************************************************
!     SAVE 1 LAYER ARRAY ON DISK
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*4 TEXT(4)
      DOUBLE PRECISION PERTIM,TOTIM
      real :: BUF(NCOL,NROW)
      REAL PERTIMS,TOTIMS
      
      integer :: ncol, kper, kstp, nrow, ilay, ichn, ncols, nrows, ic, ir
!     ------------------------------------------------------------------
!1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING
!1------INFORMATION.
      READ(ICHN) KSTP,KPER,PERTIMS,TOTIMS,TEXT,NCOLs,NROWs,ILAY
!
!2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES
!2------THE ARRAY IS DIMENSIONED (NCOL,NROW)
      READ(ICHN) ((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)
!
      PERTIM = PERTIMS
      TOTIM = TOTIMS
!
!3------RETURN
      RETURN
    END SUBROUTINE ULASAVRD

    SUBROUTINE ULASAVURD(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,&
                        NNDLAY,ILAY,ICHN,NODES)
!     ******************************************************************
!     READ SAVED 1 LAYER ARRAY ON DISK FOR UNSTRUCTURED FORMAT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DOUBLE PRECISION PERTIM,TOTIM
      real :: BUF(NODES)
      REAL PERTIMS,TOTIMS
      
      integer :: nstrt, kstp, kper, nodes, ilay, ichn, nndlay, i
!     ------------------------------------------------------------------
!
!1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING INFORMATION.
      READ(ICHN) KSTP,KPER,PERTIMS,TOTIMS,TEXT,NSTRT,NNDLAY,ILAY
!
!2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES FOR THE LAYER
      READ(ICHN) (BUF(I),I=NSTRT,NNDLAY)
      PERTIM = PERTIMS
      TOTIM = TOTIMS
!
!3------RETURN
      RETURN
      END SUBROUTINE ULASAVURD
    
    
   
    subroutine ReadBAS6_Options(Modflow)
        implicit none

        type (ModflowProject) Modflow
        
        CHARACTER*80 HEADNG(2)
        integer :: ICHFLG 
        integer :: IPRTIM 
        integer :: IFRCNVG
        integer :: LLOC   
        integer :: ISTART 
        integer :: ISTOP  
        integer :: N      
        real :: R         
        integer :: INOC   
       
        character(4000) :: line
    
        !INBAS=Modflow.iBAS6
        IOUT=FNumEco
        
        !5------Read first lines of BAS Package file and identify grid type and options.
        !5A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
        HEADNG(1)=' '
        HEADNG(2)=' '
        WRITE(IOUT,*)
        READ(INBAS,'(A)') LINE
        IF(LINE(1:1).NE.'#') GO TO 20
        HEADNG(1)=LINE(1:80)
        WRITE(IOUT,'(1X,A)') HEADNG(1)
        READ(INBAS,'(A)') LINE
        IF(LINE(1:1).NE.'#') GO TO 20
        HEADNG(2)=LINE(1:80)
        WRITE(IOUT,'(1X,A)') HEADNG(2)
        CALL URDCOM(INBAS,IOUT,LINE)
        !
        !5B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.
        20 IXSEC=0
        ICHFLG=0
        IFREFM=0
        IPRTIM=0
        IUNSTR=0
        IFRCNVG=0
        IDPIN = 0
        IDPOUT = 0
        IHMSIM = 0
        IUIHM = 0
        ISYALL = 0
        LLOC=1
        IPRCONN=0
        25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
        IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
            IXSEC=1
            Modflow.xsection=.true.

        ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
            ICHFLG=1
            Modflow.chtoch=.true.

        ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
            IFREFM=1
            Modflow.free=.true.
            WRITE(IOUT,26)
            26    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
            IPRTIM=1
            Modflow.printtime=.true.
            WRITE(IOUT,7)
            7    FORMAT(1X,'THE PRINTTIME OPTION HAS BEEN SELECTED')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'UNSTRUCTURED') THEN
            IUNSTR=1
            Modflow.unstructured=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTFV') THEN
            IPRCONN=1
            Modflow.printfv=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'CONVERGE') THEN
            IFRCNVG=1
            Modflow.converge=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'RICHARDS') THEN
            IUNSat=1
            Modflow.richards=.true.
            WRITE(IOUT,8)
            8    FORMAT(1X,'RICHARDS EQUATION SOLUTION')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'DPIN') THEN
            !----READ OPTIONS FOR SINGLE OR DOUBLE PRECISION READ / WRITE
            WRITE(IOUT,115)
            115       FORMAT(1X,'INPUT BINARY FILES FOR PRIMARY VARIABLES ',&
            'WILL BE IN DOUBLE PRECISION')
            IDPIN = 1
            Modflow.dpin=.true.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DPOUT') THEN
            WRITE(IOUT,116)
            116      FORMAT(1X,'OUTPUT BINARY FILES FOR PRIMARY VARIABLES ',&
            'WILL BE IN DOUBLE PRECISION')
            IDPOUT = 1
            Modflow.dpout=.true.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DPIO') THEN
            WRITE(IOUT,115)
            WRITE(IOUT,116)
            IDPIN = 1
            IDPOUT = 1
            Modflow.dpio=.true.
            !-----READ OPTIONS FOR IHM SIMULATION
        ELSE IF(LINE(ISTART:ISTOP).EQ.'IHM') THEN
            IHMSIM = 1
            Modflow.ihm=.true.
            WRITE(IOUT,117)
            117      FORMAT(1X,'MODFLOW-USG IS PART OF AN INTEGRATED HYDROLOGIC',1X,&
            'MODEL (IHM) SIMULATION')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUIHM,R,IOUT,INOC)
            IF(IUIHM.GT.0) THEN
                WRITE (IOUT,118) IUIHM
                118        FORMAT(1X,'IHM SIMULATION DEBUGGING INFORMATION IS WRITTEN'&
                1X,'TO FILE ON FORTRAN UNIT' I5)
            ELSE
                WRITE (IOUT,119)
                119        FORMAT(1X,'IHM SIMULATION DEBUGGING INFORMATION IS NOT'&
                1X,'WRITTEN')
            end if
        ELSE IF(LINE(ISTART:ISTOP).EQ.'SY-ALL') THEN
            ISYALL = 1
            Modflow.syall=.true.
            WRITE(IOUT,120)
            120      FORMAT(1X,'IHM SIMULATION REPLACES SY FOR ALL LAYERS')
        END IF
        IF(LLOC.LT.200) GO TO 25
        !!5C-------SET UNSTRUCTURED FLAG IF DISU IS USED
        !INDIS=IUDIS
        !IF(IUNIT(IUDIS+1).GT.0) THEN
        !    IUNSTR=1
        !    INDIS=IUDIS+1
        !end if
        !
        !5D-----PRINT A MESSAGE SHOWING OPTIONS.
        IF(IXSEC.NE.0) WRITE(IOUT,61)
        61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
        IF(ICHFLG.NE.0) WRITE(IOUT,62)
        62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
        IF(IUNSTR.NE.0) WRITE(IOUT,63)
        63 FORMAT(1X,'THE UNSTRUCTURED GRID OPTION HAS BEEN SELECTED')
    end subroutine ReadBAS6_Options
    
    SUBROUTINE ReadBAS6_IBOUND_IHEADS(Modflow)
        !     ******************************************************************
        !     Read IBOUND, HNOFLO and initial heads for unstructured grid input
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY:NLAY,&
                        INBAS,IFREFM,NODES,IOUT,&
                        NIUNIT,HNEW,NODLAY,&
                        IBOUND,STRT,&
                        IDPIN
        USE GWFBASMODULE,ONLY: HNOFLO
                    
        implicit none

        type (ModflowProject) Modflow
        
        integer :: Kloc, KK, nndlay, nstrt,ndslay,n
       
        REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
        REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
        CHARACTER*24 ANAME(2)
        DATA ANAME(1) /'          BOUNDARY ARRAY'/
        DATA ANAME(2) /'            INITIAL HEAD'/

        !INBAS=Modflow.iBAS6
        IOUT=FNumEco

        !     ------------------------------------------------------------------
        !
        !1------READ BOUNDARY ARRAY(IBOUND).
        DO Kloc = 1,NLAY
            KK = Kloc
            NNDLAY = NODLAY(Kloc)
            NSTRT = NODLAY(Kloc-1)+1
            NDSLAY = NNDLAY - NODLAY(Kloc-1)
            CALL U1DINT(IBOUND(NSTRT),ANAME(1),NDSLAY,Kloc,INBAS,IOUT)
        end do
        !
        !----------------------------------------------------------------------
        !2------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
        IF(IFREFM.EQ.0) THEN
            READ(INBAS,'(F10.0)') HNOFLO
        ELSE
            READ(INBAS,*) HNOFLO
        END IF
        WRITE(IOUT,3) HNOFLO
        3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',G12.5,&
        ' AT ALL NO-FLOW NODES (IBOUND=0).')
        !
        !-----------------------------------------------------------------------
        !3------READ INITIAL HEADS.
        IF(IDPIN.EQ.0) THEN !----------------------------------SINGLE PRECISION READ
            ALLOCATE(HTMP1(Nodes))
            DO Kloc=1,NLAY
                NNDLAY = NODLAY(Kloc)
                NSTRT = NODLAY(Kloc-1)+1
                CALL U1DREL(Htmp1(NSTRT),ANAME(2),NNDLAY-NSTRT+1,Kloc,INBAS,IOUT)
            end do
            DO N=1,NODES
                HNEW(N) = HTMP1(N)
                STRT(N) = HNEW(N)
                IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
            end do
            DEALLOCATE(HTMP1)
        ELSE       !----------------------------------DOUBLE PRECISION READ
            ALLOCATE(HTMP18(Nodes))
            DO Kloc=1,NLAY
                NNDLAY = NODLAY(Kloc)
                NSTRT = NODLAY(Kloc-1)+1
                CALL U1DREL8(Htmp18(NSTRT),ANAME(2),NNDLAY-NSTRT+1,Kloc,INBAS,IOUT)
            end do
            DO N=1,NODES
                HNEW(N) = HTMP18(N)
                STRT(N) = HNEW(N)
                IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
            end do
            DEALLOCATE(HTMP18)
        end if

        do n=1,NODES
            modflow.GWF.ibound(n)=ibound(n)
            modflow.GWF.hnew(n)=hnew(n)
        end do

        
        !
        !----------------------------------------------------------------------
        !4------RETURN.
        RETURN
    END SUBROUTINE ReadBAS6_IBOUND_IHEADS

    subroutine ReadDISU_pt1(Modflow)
       
        !      SUBROUTINE SDIS2GLO8AR (IUDIS,IOUT)
        !     *****************************************************************
        !     READ GLOBAL DATA ALLOCATE SPACE FOR 3-D DOMAIN PARAMETERS,
        !     AND READ CONFINING BED INFORMATION ARRAY, LAYCBD
        !     *****************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,&
        LENUNI,NODES,LAYCBD,&
        PERLEN,NSTP,TSMULT,ISSFLG,BOT,TOP,IUNSTR,&
        NJA,NJAG,IVSD,&
        ICONCV,NOCVCO,IDSYMRD,&
        NOVFC

        implicit none

        CHARACTER*400 LINE

        type (ModflowProject) Modflow
        
        integer :: indis,lloc,istop,istart, k
        real :: r
        
        IOUT=FNumEco

        INDIS=Modflow.iDISU
        IF(INDIS.LE.0) THEN
            WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
            CALL USTOP(' ')
        END IF
        !2-------IDENTIFY PACKAGE
        WRITE(IOUT,11) INDIS
        11 FORMAT(1X,/1X,'DIS -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',&
        ' VERSION 1 : 5/17/2010 - INPUT READ FROM UNIT ',I4)
        !
        !
        !3------Read comments and the first line following the comments.
        CALL URDCOM(INDIS,IOUT,LINE)
        !
        !4------Get the grid size, stress periods, and options like
        !4------ITMUNI, and LENUNI from first line.
        LLOC=1
        IVSD=0
        IF(IUNSTR.EQ.0)IVSD = -1
        IF(IUNSTR.EQ.0)THEN
            !4A-----FOR STRUCTURED GRID READ NLAY, NROW AND NCOL
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
            NODES = NCOL*NROW*NLAY
            !
            WRITE(IOUT,15) NLAY,NROW,NCOL
            15   FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
        ELSE
            !4B------FOR UNSTRUCTURED GRID READ NUMBER OF NODES, LAYERS AND CONNECTIVITY SIZES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NODES,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NJAG,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IVSD,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDSYMRD,R,IOUT,INDIS)
            !
            NJA = NJAG
            WRITE(IOUT,16) NODES,NLAY,NJAG,IVSD
            16   FORMAT(1X,I10,' NODES',I10,' NLAY',I10,' NJAG',&
            2X,'VERT. SUBDISCRETIZATION INDEX, IVSD = ',I2)
            WRITE(IOUT,17)IDSYMRD
            17      FORMAT(1X,'INDEX FOR INPUT OF UNSTRUCTURED, FINITE-VOLUME',1X,&
            'CONNECTIVITY INFORMATION, IDSYMRD = ',I3)
        end if
        !
        WRITE(IOUT,20) NPER
20      FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
        modflow.nPeriods=NPER
        !
        !5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
        IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
        IF(ITMUNI.EQ.0) THEN
            WRITE(IOUT,30)
            30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
        ELSE IF(ITMUNI.EQ.1) THEN
            WRITE(IOUT,40)
            40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
        ELSE IF(ITMUNI.EQ.2) THEN
            WRITE(IOUT,50)
            50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
        ELSE IF(ITMUNI.EQ.3) THEN
            WRITE(IOUT,60)
            60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
        ELSE IF(ITMUNI.EQ.4) THEN
            WRITE(IOUT,70)
            70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
        ELSE
            WRITE(IOUT,80)
            80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
        END IF
        !
        !6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
        IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
        IF(LENUNI.EQ.0) THEN
            WRITE(IOUT,90)
            90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
        ELSE IF(LENUNI.EQ.1) THEN
            WRITE(IOUT,91)
            91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
        ELSE IF(LENUNI.EQ.2) THEN
            WRITE(IOUT,93)
            93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
        ELSE IF(LENUNI.EQ.3) THEN
            WRITE(IOUT,95)
            95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
        END IF
        !7----ALLOCATE SPACE FOR TEMPORAL INFORMATION AND CONFINING LAYERS
        ALLOCATE(LAYCBD(NLAY))
        ALLOCATE(BOT(NODES))
        ALLOCATE(TOP(NODES))
        ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
        !ALLOCATE (ICONCV,NOCVCO,NOVFC)
        !
        !8----SET FLAGS AND CONFINING INFORMATION
        ICONCV=1
        NOCVCO=1
        NOVFC=0
        !
        !9-------Read confining bed information
        READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
        LAYCBD(NLAY)=0
        WRITE(IOUT,*) ' Confining bed flag for each layer:'
        WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
        !
        !10------Count confining beds and setup LAYCBD to be the confining
        !10------bed number for each layer.
        NCNFBD=0
        DO 100 K=1,NLAY
            IF(LAYCBD(K).NE.0) THEN
                NCNFBD=NCNFBD+1
                LAYCBD(K)=NCNFBD
            END IF
        100 CONTINUE
        NBOTM=NLAY+NCNFBD
        
        RETURN

    end subroutine ReadDISU_pt1

    subroutine ReadDISU_pt2(Modflow)
        implicit none

        type (ModflowProject) Modflow
        
 
        REAL, DIMENSION(:),    ALLOCATABLE  ::TEMP
        CHARACTER*24 ANAME(6)
        DATA ANAME(1) /'  NO. OF NODES PER LAYER'/
        DATA ANAME(2) /'                     TOP'/
        DATA ANAME(3) /'                     BOT'/
        DATA ANAME(4) /'                    AREA'/
        DATA ANAME(5) /'                      IA'/
        DATA ANAME(6) /'                      JA'/
        
        integer :: indis, kk, nndlay,nstrt,ndslay
        integer :: ij, ija, ii, k
        
        indis=Modflow.iDISU
        !
        !------------------------------------------------------------------
        !1-------FILL NODLAY ARRAY WITH LAST NODE NUMBER FOR EACH LAYER AND SET MXNODLAY
        ALLOCATE(NODLAY(0:NLAY))
        !1A----READ NUMBER OF NODES FOR EACH LAYER
        K = 0
        CALL U1DINT(NODLAY(1),ANAME(1),NLAY,K,INDIS,IOUT)
        !1B-----FIND MXNODLAY
        MXNODLAY = 0
        DO K=1,NLAY
            IF(NODLAY(K).GT.MXNODLAY) MXNODLAY = NODLAY(K)
        end do
        !1C------COMPUTE CUMULATIVE TO GIVE NODE NUMBER OF LAST NODE OF A LAYER
        NODLAY(0) = 0
        DO K=2,NLAY
            NODLAY(K) = NODLAY(K-1) + NODLAY(K)
        end do
        !---------------------------------------------------------------------------
        !
        !2------READ TOP ARRAY
        DO K = 1,NLAY
            KK = K
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL8(TOP(NSTRT),ANAME(2),NDSLAY,K,INDIS,IOUT)
        end do
        !
        !3------READ BOT ARRAY
        DO K = 1,NLAY
            KK = K
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL8(BOT(NSTRT),ANAME(3),NDSLAY,K,INDIS,IOUT)
        end do
        !
        !4-----READ HORIZONTAL AREA
        IF(IVSD.EQ.-1)THEN
        !4A-------READ AREA ONLY FOR ONE LAYER IF IVSD = -1
            ALLOCATE (TEMP(NODLAY(1)))
            CALL U1DREL(TEMP,ANAME(4),NODLAY(1),1,INDIS,IOUT)
            DO IJ=1,NODLAY(1)
                AREA(IJ) = TEMP(IJ)
            end do
            DEALLOCATE(TEMP)
            DO K=2,NLAY
                DO IJ=1,NODLAY(1)
                    NN = (K-1)*NODLAY(1) + IJ
                    AREA(NN) = AREA(IJ)
                end do
            end do
        ELSE
        !4B------IF IVSD IS NOT -1, READ AREA FOR EACH LAYER
            ALLOCATE (TEMP(NODES))
            DO K = 1,NLAY
                KK = K
                NNDLAY = NODLAY(K)
                NSTRT = NODLAY(K-1)+1
                NDSLAY = NNDLAY - NODLAY(K-1)
                CALL U1DREL(TEMP(NSTRT),ANAME(4),NDSLAY,K,INDIS,IOUT)
                DO IJ = NSTRT,NNDLAY
                    AREA(IJ) = TEMP(IJ)
                end do
            end do
            DEALLOCATE(TEMP)
        end if
        !
        !5------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA AND JA ARRAYS FOR GWF DOMAIN
        K = 0
        CALL U1DINT(IA,ANAME(5),NODES,K,INDIS,IOUT)
        ALLOCATE(JA(NJA))
        CALL U1DINT(JA,ANAME(6),NJA,K,INDIS,IOUT)
        !5A------ENSURE POSITIVE TERM FOR DIAGONAL OF JA
        DO IJA = 1,NJA
            IF(JA(IJA).LT.0) JA(IJA) = -JA(IJA)
        end do
        !5B------MAKE IA CUMULATIVE FROM CONNECTION-PER-NODE
        DO II=2,NODES+1
            IA(II) = IA(II) + IA(II-1)
        end do
        !---------IA(N+1) IS CUMULATIVE_IA(N) + 1
        DO II=NODES+1,2,-1
            IA(II) = IA(II-1) + 1
        end do
        IA(1) = 1
        
        !----------------------------------------------------------------------
        !15------RETURN.
        RETURN
    end subroutine ReadDISU_pt2
    
    
    SUBROUTINE ReadDISU_pt3(Modflow)
!        !     ******************************************************************
!        !     READ CLN as CL1, AND FAHL ARRAYS FOR UNSTRUCTURED GRID INPUT.
!        !     ******************************************************************
        USE GLOBAL,   ONLY:FAHL,CL1,NJAG
!
        implicit none
!
        type (ModflowProject) Modflow
        

        
        integer :: indis, k
        !
        CHARACTER*24 :: ANAME(4)
        DATA ANAME(1) /'     CONNECTION LENGTH 1'/
        DATA ANAME(2) /'     CONNECTION LENGTH 2'/
        DATA ANAME(3) /'    CONNECTION LENGTH 12'/
        DATA ANAME(4) /'      PERPENDICULAR AREA'/

        indis=Modflow.iDISU
        
        
        !12-------READ CONNECTION LENGTHS (DENOM TERM)
        ALLOCATE(CL1(NJAG))
        CL1 = 0.0
        K = 0
        CALL U1DREL(CL1,ANAME(3),NJAG,K,INDIS,IOUT)
        
        ALLOCATE(FAHL(NJAG))
        FAHL = 0.0
        CALL U1DREL(FAHL,ANAME(4),NJAG,K,INDIS,IOUT)
       
        RETURN
    END SUBROUTINE ReadDISU_pt3

    subroutine ReadDISU_StressPeriodData(Modflow)
    
        implicit none

        type (ModflowProject) Modflow
        
        integer :: indis,iss,itr,n,lloc, i
        real*8 :: pp
        integer :: istop, istart
        real :: r
        integer :: istartkp, istopkp
        real :: stofrac
        
        
        IOUT=FNumEco
        indis=Modflow.iDISU
        
        !12-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
        !12-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
        WRITE(IOUT,161)
        161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',&
                '     multiplier FOR DELT    SS FLAG',/1X,76('-'))
        ISS=0
        ITR=0
        ALLOCATE(STORFRAC(NPER))
        DO 200 N=1,NPER
            READ(INDIS,'(A)') LINE
            LLOC=1
            CALL URWORD8(LINE,LLOC,ISTART,ISTOP,3,I,PP,IOUT,INDIS)
            PERLEN(N) = PP
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
            STORFRAC(N) = -1.0
            IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
                ISSFLG(N)=0
                ITR=1
                WRITE(IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
            ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
                ISSFLG(N)=1
                ISS=1
                WRITE(IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
            ELSE IF (LINE(ISTART:ISTOP).EQ.'TRTOSS') THEN
                ISSFLG(N)=0
                ISTARTKP = ISTART
                ISTOPKP = ISTOP
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,STOFRAC,IOUT,INDIS)
                STORFRAC(N) = STOFRAC
                WRITE(IOUT,263) N,PERLEN(N),NSTP(N),TSMULT(N),&
                LINE(ISTARTKP:ISTOPKP), STOFRAC
            ELSE
                WRITE(IOUT,162)
                162    FORMAT(' SSFLAG MUST BE EITHER "SS", "TR", OR',&
                ' TRTOSS -- STOP EXECUTION (SGWF2BAS7U1ARDIS)')
                CALL USTOP(' ')
            END IF
            163 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
            263 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11,' STORAGE FRACTION =', E12.4)
            !
            !13-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
            !13-----TSMULT LE 0, OR PERLEN LT 0..
            IF(NSTP(N).LE.0) THEN
                WRITE(IOUT,164)
                164    FORMAT(1X,/1X,&
                'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
                CALL USTOP(' ')
            END IF
            ZERO=0.
            IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
                WRITE(IOUT,165)
                165    FORMAT(1X,/1X,&
                'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
                CALL USTOP(' ')
            END IF
            IF(TSMULT(N).LE.ZERO) THEN
                WRITE(IOUT,170)
                170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
                CALL USTOP(' ')
            END IF
            IF(PERLEN(N).LT.ZERO) THEN
                WRITE(IOUT,175)
                175    FORMAT(1X,/1X,&
                'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
                CALL USTOP(' ')
            END IF
        200 CONTINUE
        !
        !14-----Assign ITRSS.
        IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
            ITRSS=1
            WRITE(IOUT,270)
            270    FORMAT(/,1X,'TRANSIENT SIMULATION')
        ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
            ITRSS=0
            WRITE(IOUT,275)
            275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
        ELSE
            ITRSS=-1
            WRITE(IOUT,280)
            280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
        END IF
        !
        ALLOCATE(IA2(NEQS+1))
        ALLOCATE(IA1IA2(NEQS))
        ALLOCATE(JA2(NJA))
        ALLOCATE(JA1JA2(NJA))
        !
        !15-----RETURN.
        RETURN
    end subroutine ReadDISU_StressPeriodData



    SUBROUTINE ReadSWF(Modflow)
!     ******************************************************************
!     ALLOCATE SPACE AND READ NODE AND CONNECTIVITY INFORMATION FOR SWF DOMAIN
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE SWF1MODULE
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GLOBAL, ONLY: IOUT,NODES,IFREFM,IUNSTR,&
                       INCLN

      implicit none

      type (ModflowProject) Modflow
      
      CHARACTER*24 ANAME(3)
      CHARACTER*400 LINE
      DATA ANAME(1) /'           NODES PER SWF'/
      DATA ANAME(2) /'                      IA'/
      DATA ANAME(3) /'                      JA'/
      
      integer :: inswf, ioptfound, lloc, istart, i, in, istop
      real :: r, farea, felev, sgcl, sgcarea, smann, swfh2, swfh1
      integer :: iswfnds, k, ija, ii, iftyp, ifno, isswadi, ifgwno, ifcon, isgwadi
      
      
      IOUT=FNumEco
      INSWF=Modflow.iSWF

      
      
!      DOUBLE PRECISION FRAD
!     ------------------------------------------------------------------
!
!      IF(.NOT.ALLOCATED(NCLNNDS)) THEN
        !ALLOCATE(NCLNNDS)
        NCLNNDS = 0
!      end if
!1------IDENTIFY PACKAGE.
        !INSWF = IUNIT(IUSWF)
        WRITE(IOUT,1)INSWF
    1   FORMAT(1X,/1X,'SWF -- SURFACE WATER FLOW (SWF) DISCRETIZATION ',&
         'PROCESS, VERSION 1, 10/1/2023 INPUT READ FROM UNIT ',I4)
!
!2------ALLOCATE SCALAR VARIABLES AND INITIALIZE.
      ALLOCATE(ISWFCB,ISWFHD,ISWFDD,ISWFIB,NSWFNDS,NSWFGWC,NJA_SWF,&
       ISWFCN,ISWFMB,NSWFTYP)
      ISWFMB = 0
      ALLOCATE(ISWFPCB)
      ALLOCATE(ISWFGWCB)
      ISWFPCB=0
      ALLOCATE(ISWFTIB) !TRANSIENT IBOUND OPTION OF USGS RELEASE
      ISWFTIB=0
!
!3------SWF DIMENSIONING AND OUTPUT OPTIONS, ETC
      CALL URDCOM(INSWF,IOUT,LINE)
!3A-----CHECK FOR OPTIONS KEYWORD AT TOP OF FILE - CURRENTLY NO OPTIONS
      IPRCONN=0
      IOPTFOUND=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
! CURRENTLY NO OPTIONS - WILL HAVE AN OPTION TO MAKE TOP LAYER AS SWF DOMAIN
!      IF(LINE(ISTART:ISTOP).EQ.'OPTIONS') THEN
!        IOPTFOUND=1
!   70   CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
!        IF(LINE(ISTART:ISTOP).EQ.'TRANSIENT') THEN
!          ICLNTIB=1
!          WRITE(IOUT,71)
!   71     FORMAT(1X,'TRANSIENT IBOUND OPTION:',
!     1     ' READ TRANSIENT IBOUND RECORDS FOR EACH STRESS PERIOD.')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTIAJA') THEN
!          IPRCONN=1
!          WRITE(IOUT,72)
!   72     FORMAT(1X,'PRINT CLN IA AND JA OPTION:',
!     1     ' THE CLN IA AND JA ARRAYS WILL BE PRINTED TO LIST FILE.')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'PROCESSCCF') THEN                 !aq CLN CCF
!          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNGWCB,R,IOUT,INOC)    !aq CLN CCF
!          ICLNPCB=1                                                     !aq CLN CCF
!          WRITE(IOUT,73)                                                !aq CLN CCF
!   73 FORMAT(1X,'PROCESS CELL-TO-CELL FLOW BUDGET OPTION:',             !aq CLN CCF
!     1   ' FLOW BUDGET WILL USE A SEPARATE FILE FOR CLN-GWF FLOW.')     !aq CLN CCF
!C                                                                       !aq CLN CCF
!          IF(ICLNGWCB.LT.0) WRITE(IOUT,18)                              !aq CLN CCF
!   18 FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE PRINTED WHEN ICBCFL',   !aq CLN CCF
!     1  ' IS NOT 0 (FLAG ICLNGWCB IS LESS THAN ZERO)')                  !aq CLN CCF
!          IF(ICLNGWCB.GT.0) WRITE(IOUT,19) ICLNGWCB                     !aq CLN CCF
!   19     FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE SAVED ON UNIT ',I5,     !aq CLN CCF
!     1  '(FLAG ICLNGWCB IS GREATER THAN ZERO)')                         !aq CLN CCF
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'TOPSWF') THEN
!          ISWFTOP=1
!          WRITE(IOUT,74)
!   74 FORMAT(1X,'SURFACE WATER FLOW DOMAIN SAME AS TOP')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.' ') THEN
!          CONTINUE
!        ELSE
!          WRITE(IOUT,79) LINE(ISTART:ISTOP)
!   79     FORMAT(1X,'UNKNOWN OPTION DETECTED: ',A)
!        end if
!        IF(LLOC.LT.200) GO TO 70
!      END IF
      IF(IOPTFOUND.GT.0) CALL URDCOM(INCLN,IOUT,LINE)  ! NO OPTION NO EFFECT
!
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(8I10)') ISWFNDS,NJA_SWF,NSWFGWC,NSWFTYP,&
         ISWFCB,ISWFHD,ISWFDD,ISWFIB
          LLOC=81
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFNDS,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NJA_SWF,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWFGWC,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWFTYP,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFCB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFHD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFDD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFIB,R,IOUT,INCLN)
!sp        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRECTYP,R,IOUT,INCLN)
!SP        IF(INBCT.GT.0) THEN
!SP          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCN,R,IOUT,INCLN)
!SP        end if
!ADD----ADD NUMBER OF OTHER CLN NODE TYPES HERE TO CATALOGUE THEM
      END IF
!---------------------------------------------------------------------------
!3A-----REFLECT FLAGS IN OUTPUT LISTING FILE
    !  WRITE(IOUT,3) ISWFNDS,NSWFGWC
    !3 FORMAT(1X,'FLAG (-VE) OR NUMBER OF SWF NODES (+VE)',
    ! 1  1X,'(ISWFNDS) =',I7
    ! 1  /1X,'NUMBER OF SWF NODE TO MATRIX GRID CONNECTIONS',
    ! 1  ' (NSWFGWC) =',I7/)
!
!      IF(ISWFCB.LT.0) WRITE(IOUT,7)
!    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',
!     1   ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')
!      IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
!    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,
!     1  '(FLAG ICLNCB IS GREATER THAN ZERO)')
!      IF(ICLNCB.EQ.0) WRITE(IOUT,6)
!    6 FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',
!     1  1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')
!
   !   IF(ICLNHD.LT.0) WRITE(IOUT,9)
   ! 9 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNHD IS LESS THAN ZERO)')
   !   IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
   !10 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNHD IS GREATER THAN ZERO)')
   !   IF(ICLNHD.EQ.0) WRITE(IOUT,31)
   !31 FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')
!
   !   IF(ICLNDD.LT.0) WRITE(IOUT,12)
   !12 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNDD IS LESS THAN ZERO)')
   !   IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
   !13 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNDD IS GREATER THAN ZERO)')
   !   IF(ICLNDD.EQ.0) WRITE(IOUT,14)
   !14 FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')
!
   !   IF(ICLNIB.LT.0) WRITE(IOUT,32)
   !32 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
   !   IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
   !33 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNIB IS GREATER THAN ZERO)')
   !   IF(ICLNIB.EQ.0) WRITE(IOUT,17)
   !17 FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
!
!--------------------------------------------------------------------------------
!C3B----READ GRAVITY AND KINEMATIC VISCOSITY IN CASE IT IS REQUIRED FOR TURBULENT FLOW
!      ALLOCATE(GRAV,VISK)
!C      ALLOCATE(IBHETYP)
!      GRAV = 0.0
!      VISK = 0.0
!C      IBHETYP = 0
!   25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INCLN)
!      IF(LINE(ISTART:ISTOP).EQ.'GRAVITY') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,GRAV,IOUT,IN)
!         WRITE(IOUT,34) GRAV
!34       FORMAT(1X,'GRAVITATIONAL ACCELERATION [L/T^2] = ', G15.6)
!      ELSE IF(LINE(ISTART:ISTOP).EQ.'VISCOSITY') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,VISK,IOUT,IN)
!         WRITE(IOUT,35) VISK
!35       FORMAT(1X,'KINEMATIC VISCOSITY [L^2/T] = ', G15.6)
!      END IF
!3B----READ OPTION FOR NON-CIRCULAR CROSS-SECTIONS
!      IF(LINE(ISTART:ISTOP).EQ.'RECTANGULAR') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRECTYP,R,IOUT,IN)
!         WRITE(IOUT,36) NRECTYP
!36       FORMAT(1X,'NUMBER OF RECTANGULAR SECTION GEOMETRIES = ', I10)
!      end if
!C3C----READ OPTION FOR BHE DETAILS
!      IF(LINE(ISTART:ISTOP).EQ.'BHEDETAIL') THEN
!         IBHETYP = 1
!         IF(ITRNSP.EQ.0) IBHETYP = 0 ! NO BHE (OR INPUT) IF TRANSPORT IS NOT RUN
!         WRITE(IOUT,37)
!37       FORMAT(1X,'BHE DETAILS WILL BE INPUT FOR EACH CLN TYPE')
!      end if
!3D----READ OPTION FOR SAVING CLN OUTPUT AND UNIT NUMBER
!      IF(LINE(ISTART:ISTOP).EQ.'SAVECLNCON') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCN,R,IOUT,IN)
!         IF(INBCT.EQ.0) ICLNCN = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
!C
!        IF(ICLNCN.LT.0) WRITE(IOUT,42)
!   42   FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
!     1   'NUMBER (ISPCUN) AS USED FOR CONC OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
!     2   1X,'(FLAG ICLNCN IS LESS THAN ZERO)')
!        IF(ICLNCN.GT.0) WRITE(IOUT,43) ICLNCN
!   43   FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED ON UNIT ',I4,
!     1    '(FLAG ICLNCN IS GREATER THAN ZERO)')
!        IF(ICLNCN.EQ.0) WRITE(IOUT,44)
!   44   FORMAT(1X,'CLN CONC OUTPUT WILL NOT BE SAVED OR PRINTED',
!     1    1X,'(FLAG ICLNCN IS EQUAL TO ZERO)')
!      end if
!      IF(LINE(ISTART:ISTOP).EQ.'SAVECLNMAS') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNMB,R,IOUT,IN)
!         IF(INBCT.EQ.0) ICLNMB = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
!C
!        IF(ICLNMB.LT.0) WRITE(IOUT,45)
!   45   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED TO THE SAME',1X,
!     1   'UNIT NUMBER (IBCTCB) AS USED FOR CONC OUTPUT FOR POROUS',   !kkz - added trailing comma per JCH
!     2   1X,'MATRIX (FLAG ICLNMB IS LESS THAN ZERO)')
!        IF(ICLNMB.GT.0) WRITE(IOUT,46) ICLNMB
!   46   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED ON UNIT ',I4,
!     1    '(FLAG ICLNMB IS GREATER THAN ZERO)')
!        IF(ICLNMB.EQ.0) WRITE(IOUT,47)
!   47   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL NOT BE SAVED OR PRINTED',
!     1    1X,'(FLAG ICLNMB IS EQUAL TO ZERO)')
!      end if
!
!      IF(LLOC.LT.200) GO TO 25
!--------------------------------------------------------------------------------
!4------FOR INPUT OF MULTI-NODE WELLS OR CLN SEGMENTS
!4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
!!      IF(NCLN.GT.0)THEN
!!        ALLOCATE(NNDCLN(0:NCLN))
!!        K = 0
!!        CALL U1DINT(NNDCLN(1),ANAME(1),NCLN,K,INCLN,IOUT)
!!        NNDCLN(0) = 0
!!C
!!C5--------MAKE NNDCLN ARRAY CUMULATIVE
!!        DO I = 1,NCLN
!!          NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
!!        end do
!!        NCLNCONS = NNDCLN(NCLN)
!!C------------------------------------------------------------------------------
!!C6--------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
!!        IF(ICLNNDS.LT.0)THEN
!!C6A---------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
!!          NCLNNDS = NNDCLN(NCLN)
!!          ALLOCATE(CLNCON(NCLNNDS))
!!          DO I=1,NCLNNDS
!!            CLNCON(I) = I ! +NODES  ! (KEEP LOCAL NODE NUMBER)
!!          end do
!!        ELSE
!!C6B-------SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
!          NSWFNDS = ISWFNDS
!          ALLOCATE(SWFCON(NSWFCONS))
!          DO I=1,NCLN
!C            IF(IFREFM.EQ.0) THEN
!C              READ(INCLN,'(200I10)')
!C     1        (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
!C            ELSE
!              READ(INCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
!C            end if
!          end do
!!cspC6C---------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
!!csp          DO I=1,NCLNCONS
!!csp            CLNCON(I) = NODES + CLNCON(I)
!!csp          end do
!!        end if
!C6D--------CONVERT TO IA_CLN AND JA_CLN
!        ALLOCATE(IA_CLN(NCLNNDS+1))
!        CALL FILLIAJA_CLN
!C6E---------DEALLOCATE UNWANTED ARRAYS
!        DEALLOCATE (NNDCLN) ! NNDCLN NEEDED FOR WRITING BUDGET TO ASCII FILE?
!        DEALLOCATE (CLNCON)
!      ELSE
!----------------------------------------------------------------------
!7------FOR INPUT OF IA AND JAC OF CLN DOMAIN (NCLN = 0), READ DIRECTLY
      NSWFNDS = ISWFNDS
!      ALLOCATE(BOTSWF(NSWFNNS))
!      ALLOCATE(AREASWF(NSWFNNS))
!     READ SWF BOTTOM ELEVATION/AREA - BOTSWF,AREASWF
!      CALL U1DREL8(BOTSWF(1),ANAME(2),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DREL8(AREASWF(1),ANAME(3),NSWFNNS,0,INSWF,IOUT)
!
      ALLOCATE(IA_SWF(NSWFNDS+1))
!7B-------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA_SWF AND JA_SWF ARRAYS
        K = 0
        CALL U1DINT(IA_SWF,ANAME(2),NSWFNDS,K,INSWF,IOUT)
        ALLOCATE(JA_SWF(NJA_SWF))
        CALL U1DINT(JA_SWF,ANAME(3),NJA_SWF,K,INSWF,IOUT)
!7C--------ENSURE POSITIVE TERM FOR DIAGONAL OF JA_SWF
        DO IJA = 1,NJA_SWF
          IF(JA_SWF(IJA).LT.0) JA_SWF(IJA) = -JA_SWF(IJA)
        end do
!7D--------MAKE IA_SWF CUMULATIVE FROM CONNECTION-PER-NODE
        DO II=2,NSWFNDS+1
          IA_SWF(II) = IA_SWF(II) + IA_SWF(II-1)
        end do
!---------IA_SWF(N+1) IS CUMULATIVE_IA_SWF(N) + 1
        DO II=NSWFNDS+1,2,-1
          IA_SWF(II) = IA_SWF(II-1) + 1
        end do
        IA_SWF(1) = 1
! SWF-GWF CONNECTION

!      end if
!----------------------------------------------------------------------
!8------ALLOCATE SPACE FOR SWF PROPERTY ARRAYS
      ALLOCATE(ASWFNDS(NSWFNDS,4))
      ALLOCATE(ISSWADISWF(NSWFNDS))
      ALLOCATE(ISGWADISWF(NSWFGWC))
!------- ALLOCATE SPACE FOR SWF PROPERTY ARRAYS
!
!      ALLOCATE(MANNSWF(NSWFNNS))
!      ALLOCATE(MICHSWF(NSWFNNS))
!      ALLOCATE(CLSGSWF(NSWFNNS))
!      ALLOCATE(ISSWADISWF(NSWFNNS))
!      ALLOCATE(ISGWADISWF(NSWFNNS))
!      CALL U1DREL8(MANNSWF(1),ANAME(4),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DREL8(MICHSWF(1),ANAME(5),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DINT(ISSWADISWF(1),ANAME(6),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DINT(ISGWADISWF(1),ANAME(7),NSWFNNS,0,INSWF,IOUT)
!
!      ALLOCATE(CLSGSWF(NSWFNNS))
!      CALL U1DREL8(CLSGSWF(1),ANAME(6),NSWFNNS,0,INSWF,IOUT)
!
!9------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
!      WRITE(IOUT,21)
!21    FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/
!     1  20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,
!     1  'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,
!     1  'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),
!     1   4X,11('-'),8X,6('-'),4X,7('-'))
!
!10-------READ BASIC PROPERTIES FOR ALL SWF NODES AND FILL ARRAYS
      DO I = 1,NSWFNDS
        CALL URDCOM(INSWF,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,FAREA,FELEV,ISSWADI
          LLOC=51
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FAREA,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISSWADI,R,IOUT,INSWF)
        END IF
!        WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
!22      FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)
!C11B------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ASWFNDS(I,1) = IFNO + NODES + NCLNNDS ! GLOBAL NODE NUMBER FOR CLN-CELL
        ASWFNDS(I,2) = IFTYP
        ASWFNDS(I,3) = FAREA
        ASWFNDS(I,4) = FELEV
        modflow.SWF.zCell(i)=FELEV
        ISSWADISWF(I) = ISSWADI
      end do
!----------------------------------------------------------------------------------------
!12------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
      ALLOCATE(ASWFGWC(NSWFGWC,5))
!----------------------------------------------------------------------------------------
!13------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
      IF(IUNSTR.EQ.0)THEN
!
!14A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
!14A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION
!        CALL SSWF2DIS1SR
      ELSE
!
!14B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
!14B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION
!        CALL SCLN2DIS1UR
        DO I=1,NSWFGWC
          CALL URDCOM(INSWF,IOUT,LINE)
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFGWNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SGCL,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SGCAREA,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISGWADI,R,IOUT,INSWF)
          ASWFGWC(I,1)=IFNO
          ASWFGWC(I,2)=IFGWNO
          ASWFGWC(I,3)=IFCON
          ASWFGWC(I,4)=SGCL
          ASWFGWC(I,5)=SGCAREA
          ISGWADISWF(I)=ISGWADI
        end do
      end if
!
      ALLOCATE(ASWFCOND(NSWFTYP,4))
      DO I=1,NSWFTYP
        CALL URDCOM(INSWF,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(I10,3F10.3)') IFNO,SMANN,SWFH1,SWFH2
          LLOC=41
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SMANN,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SWFH1,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SWFH2,IOUT,INSWF)
        end if
        ASWFCOND(I,1)=IFNO
        ASWFCOND(I,2)=SMANN
        ASWFCOND(I,3)=SWFH1
        ASWFCOND(I,4)=SWFH2
      end do

!----------------------------------------------------------------------------------------
!C15A------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
!      IF(NCONDUITYP.GT.0)THEN
!        CALL SCLN2COND1RP
!      end if
!C----------------------------------------------------------------------------------------
!C15B------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
!      IF(NRECTYP.GT.0)THEN
!        CALL SCLN2REC1RP
!      end if
!----------------------------------------------------------------------------------------
!16------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
!ADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
!----------------------------------------------------------------------------------------

! ---------------------------------------------------------------------------------------
!C16------IF IPRCONN THEN WRITE CLN CONNECTIVITY TO THE OUTPUT FILE
!      IF(IPRCONN.NE.0)THEN
!        WRITE(IOUT,'(/,A)')' IA_CLN IS BELOW, 40I10'
!        WRITE(IOUT,55)(IA_CLN(I),I=1,NCLNNDS+1)
!        WRITE(IOUT,*)'NJA_CLN = ',NJA_CLN
!        WRITE(IOUT,*)'JA_CLN IS BELOW, 40I10'
!        WRITE(IOUT,55)(JA_CLN(J),J=1,NJA_CLN)
!55      FORMAT(40I10)
!      end if
!----------------------------------------------------------------------------------------
!C18-------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
!      WRITE(IOUT,*)
!      DO I = 1,NCLNNDS
!        IFDIR = ACLNNDS(I,3)
!        IF(IFDIR.EQ.2)THEN
!          IFTYP =  ACLNNDS(I,2)
!          FLENG = ACLNNDS(I,4)
!          FANGLE = ACLNNDS(I,6)
!          FDPTH = FLENG * SIN(FANGLE)
!          IC=IFTYP
!          CALL CLNR(IC,FRAD)
!          IF(FDPTH.LT.2.0*FRAD) THEN
!              IFDIR = 1
!              ACLNNDS(I,3) = IFDIR
!              IFNO = ACLNNDS(I,1) - NODES
!              WRITE(IOUT,222)IFNO
!          end if
!        end if
!222      FORMAT(5X,'ANGLED CLN CELL NO', I7,' MADE HORIZONTAL')
!      end do
!17-----RETURN
      RETURN
      END SUBROUTINE ReadSWF

    
    subroutine ReadCLN_pt2(Modflow)
        implicit none

        type (ModflowProject) Modflow
        character(400) :: line
        
        integer :: IFNO, i
        real :: FRAD
        real :: CONDUITK
        integer :: LLOC
        real :: TCOND
        real :: TTHK
        real :: TCOEF
        real :: TCFLUID
        integer :: ISTOP
        real :: R, r1, r2, r3
        integer :: ISTART
        real :: FSRAD
        real*8 :: AREAF
        real*8 :: PERIF
        
        IOUT=FNumEco

        !----------------------------------------------------------------------------------------
        !12------ALLOCATE SPACE FOR CONDUIT TYPE CLNs AND PREPARE TO REFLECT INPUT TO LISTING FILE
        ALLOCATE (ACLNCOND(NCONDUITYP,5))
        ALLOCATE (BHEPROP(NCONDUITYP,4))
        BHEPROP = 0.0
        IF(IBHETYP.EQ.0) THEN
            WRITE(IOUT,62)
62          FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
            20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
            /5X,12('-'),8X,6('-'),3X,13('-'))
            
        ELSE
            WRITE(IOUT,63)
63          FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
            20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
            5X,'COND. PIPE', 6X,'THICK PIPE',5X,'COND. FLUID',&
            5X,'CONV. COEFF',&
            /5X,12('-'),4X,10('-'),3X,13('-'))
            
        end if
        !13------READ CONDUIT PROPERTIES FOR EACH CONDUIT TYPE
        DO I=1,NCONDUITYP
            CALL URDCOM(INCLN,IOUT,LINE)
            IF(IFREFM.EQ.0) THEN
                IF(IBHETYP.EQ.0)THEN
                    READ(LINE,'(I10,2F10.3)') IFNO,FRAD,CONDUITK
                    LLOC=71
                ELSE
                    READ(LINE,'(I10,6F10.3)')IFNO,FRAD,CONDUITK,TCOND,TTHK,&
                    TCFLUID,TCOEF
                    LLOC=111
                end if
            ELSE
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSRAD,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONDUITK,IOUT,INCLN)
                
                !rgm looks like young-jin added a line of data here
                read(INCLN,'(a)') line
                istart=0
                istop=0
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r1,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r2,IOUT,INCLN)
                ! Youn-Jin: What's up with this??
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r3,IOUT,INCLN)
                
                
                
                FRAD = FSRAD
                IF(IBHETYP.EQ.1)THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOND,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTHK,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCFLUID,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOEF,IOUT,INCLN)
                end if
            END IF
            !
            !14--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
            ACLNCOND(I,1) = IFNO
            ACLNCOND(I,2) = FRAD
            ACLNCOND(I,3) = CONDUITK
            CALL CLNA(IFNO,AREAF)
            ACLNCOND(I,4) = AREAF
            CALL CLNP(I,PERIF)
            ACLNCOND(I,5) = PERIF
            IF(IBHETYP.EQ.1)THEN
                BHEPROP(I,1) = TCOND
                BHEPROP(I,2) = TTHK
                BHEPROP(I,3) = TCFLUID
                BHEPROP(I,4) = TCOEF
                WRITE(IOUT,26) IFNO,FRAD,CONDUITK,TCOND,TTHK,TCFLUID,TCOEF
26              FORMAT(5X,I10,6(1X,E15.6))
                
            ELSE
                WRITE(IOUT,25)IFNO,FRAD,CONDUITK
25              FORMAT(5X,I10,2(1X,E15.6))
                
                WRITE(IOUT,67)
67              FORMAT(/20X,' new CONDUIT NODE INFORMATION'/&
                20X,40('-')/5X,'CONDUIT NODE?',8X,'r1',3X,'r2',3X,'r3',&
                /5X,12('-'),8X,6('-'),3X,13('-'),3X,13('-'))

                
            end if
        end do
                
        continue
    end subroutine ReadCLN_pt2
    
    SUBROUTINE CLNA(IC,AREAF)
        !--------COMPUTE X-SECTIONAL FLOW AREA FOR NODE
        USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP,ACLNREC,NRECTYP
        DOUBLE PRECISION AREAF,RADFSQ
        integer :: ic
        real :: pi
        integer :: icl
        !--------------------------------------------------------------------------------------
        PI = 3.1415926
        IF(IC.LE.NCONDUITYP)THEN
            !1-------CLN NODE IS A CONDUIT
            RADFSQ = ACLNCOND(IC,2)**2
            AREAF = PI * RADFSQ
        ELSEIF(IC.GT.NCONDUITYP.AND.IC.LE.NCONDUITYP+NRECTYP)THEN
            ICL = IC - NCONDUITYP
            AREAF = ACLNREC(ICL,2) * ACLNREC(ICL,3)
        ELSEIF(IC.GT.NCONDUITYP+NRECTYP)then
            !2------ADD COMPUTATION FOR AREA FOR OTHER CLN TYPES HERE
            !ADD      ADD COMPUTATION FOR AREA FOR OTHER TYPES OF CLNs HERE
        end if
        !7------RETURN
        RETURN
    END SUBROUTINE CLNA
    
    SUBROUTINE CLNP(IC,FPER)
        !--------COMPUTE EFFECTIVE PERIMETER FOR CONNECTION OF CLN SEGMENT TO 3-D GRID
        USE CLN1MODULE, ONLY: ACLNCOND,NCONDUITYP,ACLNREC,NRECTYP
        DOUBLE PRECISION FPER
        integer :: ic
        real :: pi
        integer :: icl
        !--------------------------------------------------------------------------------------
        PI = 3.1415926
        IF(IC.LE.NCONDUITYP)THEN
            !1-------CLN NODE IS A CONDUIT
            FPER = 2 * PI * ACLNCOND(IC,2)
        ELSEIF(IC.GT.NCONDUITYP.AND.IC.LE.NCONDUITYP+NRECTYP)THEN
            !1B---------CLN1 NODE IS A RECTANGULAR SECTION
            ICL = IC - NCONDUITYP
            FPER = 2 * (ACLNREC(ICL,2) + ACLNREC(ICL,3))
        ELSEIF(IC.GT.NCONDUITYP+NRECTYP)THEN
            !2------ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE
            !ADD      ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE
        end if
        !7------RETURN
        RETURN
    END SUBROUTINE CLNP

      SUBROUTINE ReadSMS(Modflow)      ! SMS7U1AR(IN,INTIB)

      USE GLOBAL, ONLY: IOUT,&
                 NLAY,ILAYCON4,ISYMFLG,INGNCn
      USE GWFBCFMODULE, ONLY: LAYCON
      USE SMSMODULE
!sp      USE GNCMODULE, ONLY:ISYMGNC
!sp      USE GNC2MODULE, ONLY:ISYMGNC2
      USE GNCnMODULE, ONLY:ISYMGNCn

      IMPLICIT NONE

      !     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      INTRINSIC INT
      !EXTERNAL URDCOM, URWORD, UPARLSTAL
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER IN
!     ------------------------------------------------------------------
!     LOCAL VARIABLES
!     ------------------------------------------------------------------
      INTEGER lloc, istart, istop, i, K, IFDPARAM, MXVL, NPP
      INTEGER IPCGUM
      CHARACTER(LEN=200) line
      REAL r, HCLOSEdum, HICLOSEdum,  thetadum, amomentdum,yo
      REAL akappadum, gammadum, BREDUCDUM,BTOLDUM,RESLIMDUM
!
      type (ModflowProject) Modflow
      
      in=modflow.iSMS
      iout=FNumEco
!     LOCAL VARIABLES FOR GCG SOLVER

!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE AND INITIALIZE.
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'SMS -- SPARSE MATRIX SOLVER PACKAGE, VERSION 7',&
     ', 5/2/2005',/,9X,'INPUT READ FROM UNIT',I3)
      !ALLOCATE (HCLOSE, HICLOSE,BIGCHOLD,BIGCH)
      !ALLOCATE (ITER1,THETA,MXITER,LINMETH,NONMETH,IPRSMS)
      !ALLOCATE (Akappa,Gamma,Amomentum,Breduc,Btol,RES_LIM,&
      ! Numtrack,IBFLAG)
      ! DM: Allocate forcing term variables
      !ALLOCATE (Rcutoff,ForcingAlpha,ForcingGamma,MaxRcutoff)
      !ALLOCATE (ICUTOFF,NoMoreRcutoff,ITRUNCNEWTON)
      ITRUNCNEWTON = 0
      Rcutoff = -1.0
      ForcingAlpha = (1.0 + sqrt(5.0)) / 2.0
      ForcingGamma = 0.9
      MaxRcutoff = 0.9
      ICUTOFF = 0
      NoMoreRcutoff = 0
      ! End DM
!
      ISOLVEACTIVE=0
      IBOTAV = 0
      ISHIFT = 0
      i = 1
      THETA = 1.0
      Akappa = 0.0
      Gamma = 0.0
      Amomentum = 0.0
      Numtrack = 0
      Btol = 0
      Breduc = 0.
      RES_LIM = 0.
      IBFLAG = 0
! Check if default solver values will be used
      lloc = 1
      IFDPARAM = 0
      CALL URDCOM(In, IOUT, line)
      NPP = 0
      MXVL = 0
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SIMPLE') THEN
        IFDPARAM = 1
         WRITE(IOUT,21)
   21    FORMAT(1X,'SIMPLE OPTION:',/,&
          1x,'DEFAULT SOLVER INPUT VALUES FOR FAST SOLUTIONS')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MODERATE') THEN
         IFDPARAM=2
         WRITE(IOUT,23)
   23    FORMAT(1X,'MODERATE OPTION:',/,1X,'DEFAULT SOLVER',&
              ' input VALUES REFLECT MODERETELY NONLINEAR MODEL')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPLEX') THEN
         IFDPARAM=3
         WRITE(IOUT,25)
   25    FORMAT(1X,'COMPLEX OPTION:',/,1X,'DEFAULT SOLVER',&
      ' INPUT VALUES REFLECT STRONGLY NONLINEAR MODEL')
      ELSE
        BACKSPACE IN
        WRITE(IOUT,27)
   27   FORMAT(1X, ' ALL SOLVER INPUT DATA WILL BE READ',&
                          1X,'FROM THE SOLVER INPUT FILE. ')
      END IF
!2------Read nonlinear iteration parameters and linear solver selection index
      lloc = 1
      CALL URDCOM(In, Iout, line)
      CALL URWORD(line, lloc, istart, istop, 3, i, HCLOSEdum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, HICLOSEdum, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, MXITER, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, ITER1, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, IPRSMS, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Nonmeth, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Linmeth, r, Iout, In)
!2B----READ OPTIONS
   30 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SOLVEACTIVE') THEN
        ISOLVEACTIVE=1
        WRITE(IOUT,31)
   31   FORMAT(1X,'ONLY ACTIVE NODES WILL BE PASSED TO THE SOLVER')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'DAMPBOT') THEN
        IBOTAV=1
        WRITE(IOUT,32)
   32   FORMAT(1X,'BOTTOM DAMPING APPLIED TO EACH LINEAR SOLUTION')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'SHIFT') THEN
        ISHIFT=1
        WRITE(IOUT,33)
   33   FORMAT(1X,'SOLUTION VECTOR WILL BE SHIFTED BEFORE AND AFTER',&
         1X,'EACH LINEAR SOLVE')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'TRUNCATEDNEWTON') THEN
        ITRUNCNEWTON=1
        WRITE(IOUT,34)
   34   FORMAT(1X,'TRUNCATED NEWTON OPTION WILL BE USED')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'TRUNCATEDNEWTONCUTOFF') THEN
        ITRUNCNEWTON=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,YO,IOUT,IN)
        MaxRcutoff = YO
        WRITE(IOUT,35) YO
   35   FORMAT(1X,'TRUNCATED NEWTON OPTION WILL BE USED WITH MAXCUTOFF'&
          1X,'=', E12.5)
      end if
      IF(LLOC.LT.200) GO TO 30
!
!      IF(NONMETH.NE.0) IBOTAV = 1  ! DO LIKE IN VERSION 1.4 OF MODFLOW-USG
      IF(NONMETH.NE.0)THEN
        IF ( IFDPARAM.EQ.0 ) THEN
        lloc = 1
        CALL URDCOM(In, Iout, line)
        CALL URWORD(line, lloc, istart, istop, 3, i, thetadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,akappadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i, gammadum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3,i,amomentdum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, Numtrack, r, Iout, In)
        Theta = Thetadum
        Akappa = akappadum
        Gamma = gammadum
        Amomentum = amomentdum
        IF( NUMTRACK.GT.0 ) THEN
        CALL URWORD(line, lloc, istart, istop, 3, i,  Btoldum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,Breducdum, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,RESLIMDUM, Iout, In)
        Btol = Btoldum
        Breduc = Breducdum
        RES_LIM = RESLIMDUM
        end if
        ELSE
        CALL SET_RELAX(IFDPARAM)
        END IF
        ! DM: Read truncated Newton enabled/disabled flag
!SP        CALL URWORD(line, lloc, istart, istop, 2, ITRUNCNEWTON, r,
!SP    1              Iout, In)
        ! End DM
      END IF
!
      HCLOSE = HCLOSEDUM
      HICLOSE = HICLOSEDUM
      IF ( Theta.LT.CLOSEZERO ) Theta = 1.0e-3
!
      ILAYCON4=0
      DO K=1,NLAY
        IF(LAYCON(K).EQ.4.OR.LAYCON(K).EQ.5)THEN
          ILAYCON4=1
        end if
      end do
!
!      IF(ILAYCON4.NE.1.AND.INCLN.EQ.0)THEN
!        IF(NONMETH.GT.0)NONMETH = -NONMETH
!      end if
!3------Echo input of nonlinear iteratin parameters and linear solver index
      WRITE(IOUT,9002) HCLOSE,HICLOSE,MXITER,ITER1,iprsms,&
      NONMETH,LINMETH
!
 9002 FORMAT(1X,'OUTER ITERATION CONVERGENCE CRITERION (HCLOSE) = ',&
       E15.6,&
           /1X,'INNER ITERATION CONVERGENCE CRITERION (HICLOSE) = ',&
       E15.6,&
           /1X,'MAXIMUM NUMBER OF OUTER ITERATIONS (MXITER)     = ',I9,&
           /1X,'MAXIMUM NUMBER OF INNER ITERATIONS (ITER1)      = ',I9,&
           /1X,'SOLVER PRINTOUT INDEX             (IPRSMS)      = ',I9,&
           /1X,'NONLINEAR ITERATION METHOD    (NONLINMETH)      = ',I9,&
           /1X,'LINEAR SOLUTION METHOD           (LINMETH)      = ',I9)
!
      IF(NONMETH.NE.0)THEN
        WRITE(IOUT,9003)THETA,AKAPPA,GAMMA,AMOMENTUM,NUMTRACK
        IF(NUMTRACK.NE.0) WRITE(IOUT,9004) BTOL,BREDUC,RES_LIM
        WRITE(IOUT,9005) ITRUNCNEWTON
      end if
9003  FORMAT(1X,'D-B-D WEIGHT REDUCTION FACTOR      (THETA)      = ',&
       E15.6,&
           /1X,'D-B-D WEIGHT INCREASE INCREMENT    (KAPPA)      = ',&
       E15.6,&
           /1X,'D-B-D PREVIOUS HISTORY FACTOR      (GAMMA)      = ',&
       E15.6,&
           /1X,'MOMENTUM TERM                  (AMOMENTUM)      = ',&
       E15.6,&
           /1X,'MAXIMUM NUMBER OF BACKTRACKS    (NUMTRACK)      = ',I9)
9004  FORMAT(1X,'BACKTRACKING TOLERANCE FACTOR       (BTOL)      = ',&
       E15.6,&
           /1X,'BACKTRACKING REDUCTION FACTOR     (BREDUC)      = ',&
       E15.6,&
           /1X,'BACKTRACKING RESIDUAL LIMIT      (RES_LIM)      = ',&
       E15.6)
9005  FORMAT(1X,'TRUNCATED NEWTON FLAG     (ITRUNCNEWTON)      = ',I9)
      IF(MXITER.LE.0) THEN
        WRITE(*,5)
        CALL USTOP(' ')
      ELSEIF(ITER1.LE.0) THEN
        WRITE(*,7)
        CALL USTOP(' ')
      end if
    5 FORMAT(/1X,'ERROR: OUTER ITERATION NUMBER MUST BE > 0.')
    7 FORMAT(/1X,'ERROR: INNER ITERATION NUMBER MUST BE > 0.')
!
      ISYMFLG = 1
      IF ( Nonmeth.GT.0 )Then
        Write(iout,*) '***Newton Linearization will be used***'
        Write(iout,*)
        ISYMFLG = 0
      ELSEIF ( Nonmeth.EQ.0 )Then
        Write(iout,*) '***Picard Linearization will be used***'
        Write(iout,*)
      ELSEIF ( Nonmeth.LT.0 )Then
        Write(iout,*) '***Picard Linearization will be used with relaxat&
     ion***'
        Write(iout,*)
      ELSE
        Write(iout,*) '***Incorrect value for variable Nonmeth was ',&
                     'specified. Check input.***'
        Write(iout,*)
        Call USTOP('  ')
      END IF
!CCC
!CCC-----SET ISOLVEACTIVE
!CC      IF ( Linmeth==2 )Then
!CC        IF(ISOLVEACTIVE.EQ.1) THEN
!CC          ISOLVEACTIVE=0
!CC          WRITE(IOUT,'(2A)') 'SOLVEACTIVE DOES NOT WORK WITH PCGU ',
!CC     1      'LINEAR SOLVER. SOLVEACTIVE DISABLED.'
!CC        end if
!CC      end if
!
!!-----IF SOLVEACTIVE=0 SET IA2, JA2 HERE
!      IF(ISOLVEACTIVE.EQ.0) CALL SMS_REDUCE0()
!!
!4------Call secondary subroutine to initialize and read linear solver parameters
      IF ( Linmeth==1 )Then
!4a-------for XMD solver
        Write(iout,*) '***XMD linear solver will be used***'
        !CALL XMD7U1AR(IN,IFDPARAM)
        Write(iout,*)
        ISYMFLG = 0
        !IF(IACL.EQ.0) ISYMFLG = 1
      ELSEIF ( Linmeth==2 )Then
!4b-------for pcgu solver
        Write(iout,*) '***PCGU linear solver will be used***'
        !CALL PCGU7U1AR(IN, NJA, NEQS, MXITER, HICLOSE, ITER1, IPRSMS,&
        !              IFDPARAM, IPCGUM)
        Write(iout,*)
        ISYMFLG = 0
        IF (IPCGUM.EQ.1) ISYMFLG = 1
      ELSEIF ( Linmeth==4 )Then
!---------for ppcgu solver
        Write(iout,*) '***Parallel PCGU linear solver will be used***'
!sp        CALL PPCGU1AR(IN, NJA, NEQS, MXITER, HICLOSE, ITER1, IPRSMS,
!sp     +                 IFDPARAM, IPCGUM)
        Write(iout,*)
        ISYMFLG = 0
        IF ( IPCGUM.EQ.1 ) ISYMFLG = 1
      ELSE
!4c-----Incorrect linear solver flag
        Write(iout,*) '***Incorrect value for Linear solution method ',&
                     'specified. Check input.***'
        Write(iout,*)
        Call USTOP('  ')
      END IF
!sp      IF(INGNC.NE.0.)THEN
!sp        IF(ISYMGNC.EQ.0.AND.ISYMFLG.EQ.1)THEN
!sp          WRITE(IOUT,*) '***ISYMGNC and ISYMFLG mismatch, unsymmetric
!sp     1 option selected with symmetric solver. Stopping.***'
!sp          STOP
!sp        end if
!sp      end if
!sp      IF(INGNC2.NE.0.)THEN
!sp        IF(ISYMGNC2.EQ.0.AND.ISYMFLG.EQ.1)THEN
!sp          WRITE(IOUT,*) '***ISYMGNC and ISYMFLG mismatch, unsymmetric
!sp     1 option selected with symmetric solver. Stopping.***'
!sp          STOP
!sp        end if
!sp      end if
      IF(INGNCn.NE.0.)THEN
        IF(ISYMGNCn.EQ.0.AND.ISYMFLG.EQ.1)THEN
          WRITE(IOUT,*) '***ISYMGNCn and ISYMFLG mismatch, unsymmetric&
      option selected with symmetric solver. Stopping.***'
          STOP
        end if
      end if
!!
!!---------------------------------------------------------------------------------
!!5-----Allocate space for nonlinear arrays and initialize
!      ALLOCATE(HTEMP(NEQS))
!      ALLOCATE (Hncg(MXITER),Lrch(3,MXITER))
!      ALLOCATE (HncgL(MXITER),LrchL(MXITER))
!!      IF(NONMETH.GT.0)THEN
!        ALLOCATE (AMATFL(NJA))
!!      ELSE
!!        AMATFL => AMAT
!!      end if
!      IF(IABS(NONMETH).EQ.1)THEN
!        ALLOCATE (Wsave(NEQS),hchold(NEQS),DEold(NEQS))
!        WSAVE = 0.
!        HCHOLD = 0.
!        DEold = 0.
!      end if
!      Hncg = 0.0D0
!      LRCH = 0
!      HncgL = 0.0D0
!      LRCHL = 0
!!
!      IF(ISOLVEACTIVE.EQ.1) THEN
!        IF(INTIB.GT.0) ISOLVEACTIVE=2
!        IF(IWDFLG.NE.0) ISOLVEACTIVE=3
!      end if
!!-----IF SOLVEACTIVE=1 SET IA2, JA2 HERE FOR STATIC IBOUND
!      IF(ISOLVEACTIVE.EQ.1) CALL SMS_REDUCE()
!! ----------------------------------------------------------------------
!!-------SET BOTMIN FOR NEWTON DAMPENING IF IBOTAV = 1 (do even if not IBOTAV=1)
!!      IF (IBOTAV.EQ.0) THEN
!!        ALLOCATE(CELLBOTMIN(1))
!!      ELSE
!        ALLOCATE(CELLBOTMIN(NODES))
!!---------INITIALIZE CELLBOTM TO BOTTOM OF CELL
!        DO N = 1, NODES
!          CELLBOTMIN(N) = BOT(N)
!        END DO
!!---------USE BOTTOM OF MODEL FOR CONSTANTCV MODELS
!        IF (ICONCV.NE.0) THEN
!          DO K = NLAY, 1, -1
!            NNDLAY = NODLAY(K)
!            NSTRT = NODLAY(K-1)+1
!            DO N = NNDLAY, NSTRT, -1
!              BBOT = BOT(N)
!              IF (CELLBOTMIN(N) < BBOT) THEN
!                BBOT = CELLBOTMIN(N)
!              END IF
!!---------------PUSH THE VALUE UP TO OVERLYING CELLS IF
!!               BBOT IS LESS THAN THE CELLBOTMIN IN THE
!!               OVERLYING CELL
!              I0 = IA(N) + 1
!              I1 = IA(N+1) - 1
!              DO J = I0, I1
!                JCOL = JA(J)
!                JCOLS = JAS(J)
!                IF (JCOL < N .AND. IVC(JCOLS).EQ.1) THEN
!                  IF (BBOT < CELLBOTMIN(JCOL)) THEN
!                    CELLBOTMIN(JCOL) = BBOT
!                  END IF
!                END IF
!              END DO
!            END DO
!          END DO
!        END IF
!!      END IF
! ----------------------------------------------------------------------
!6------Return
      RETURN 
      END SUBROUTINE ReadSMS
      
      SUBROUTINE SET_RELAX(IFDPARAM)
      USE SMSMODULE, ONLY: Akappa,Gamma,Amomentum,Breduc,Btol,Numtrack,&
                          THETA, Res_lim
      INTEGER IFDPARAM
! Simple option
      SELECT CASE ( IFDPARAM )
      CASE ( 1 )
        Theta = 1.0
        Akappa = 0.0
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 0
        Btol = 0.0
        Breduc = 0.0
        Res_lim = 0.0
! Moderate
       CASE ( 2 )
        Theta = 0.9
        Akappa = 0.0001
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 0
        Btol = 0.0
        Breduc = 0.0
        Res_lim = 0.0
! Complex
       CASE ( 3 )
        Theta = 0.8
        Akappa = 0.0001
        Gamma = 0.0
        Amomentum = 0.0
        Numtrack = 20
        Btol = 1.05
        Breduc = 0.1
        Res_lim = 0.002
      END SELECT
      RETURN
      END

   
    
    subroutine Read_GWF_GSF(Modflow)
        implicit none
        
        type (ModflowProject) Modflow

        integer :: i, j
        
        integer :: i1, i2, i3
        real :: r1, r2, r3
        
        ! read initial comment lines beginning with #
        do 
            read(Modflow.iGSF,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            end if
            backspace(Modflow.iGSF)
            exit
        end do

        read(Modflow.iGSF,'(a)') Modflow.GWF.meshtype
        if(Modflow.GWF.meshtype=='UNSTRUCTURED(NODALCONTROLVOLUME)') then
            Modflow.NodalControlVolume=.true.
            read(Modflow.iGSF,*) Modflow.GWF.nElements, Modflow.GWF.nLayers, Modflow.GWF.iz, Modflow.GWF.ic
            read(Modflow.iGSF,*) Modflow.GWF.nNodes
            Modflow.GWF.nCells=Modflow.GWF.nNodes
        else
            read(Modflow.iGSF,*) Modflow.GWF.nElements, Modflow.GWF.nLayers, Modflow.GWF.iz, Modflow.GWF.ic
            read(Modflow.iGSF,*) Modflow.GWF.nNodes
            Modflow.GWF.nCells=Modflow.GWF.nElements
        end if

        
        
        allocate(Modflow.GWF.x(Modflow.GWF.nNodes),Modflow.GWF.y(Modflow.GWF.nNodes),Modflow.GWF.z(Modflow.GWF.nNodes), stat=ialloc)
        call AllocChk(ialloc,'GWF node coordinate arrays')
        Modflow.GWF.x = 0 ! automatic initialization
        Modflow.GWF.y = 0 ! automatic initialization
        Modflow.GWF.z = 0 ! automatic initialization
        
        read(Modflow.iGSF,*) (Modflow.GWF.x(i),Modflow.GWF.y(i),Modflow.GWF.z(i),i=1,Modflow.GWF.nNodes)

        ! determine the number of nodes per cell (Modflow.GWF.nNodesPerCell)
        if(Modflow.NodalControlVolume) then
            read(Modflow.iGSF,*) i1,Modflow.GWF.nNodesPerCell
        else
            read(Modflow.iGSF,*) i1,r1,r2,r3,i2,Modflow.GWF.nNodesPerCell
        endif
        backspace(Modflow.iGSF)
 
        allocate(Modflow.GWF.iNode(Modflow.GWF.nNodesPerCell,Modflow.GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,'GWF iNode arrays')
        
        allocate(Modflow.GWF.xCell(Modflow.GWF.nCells),Modflow.GWF.yCell(Modflow.GWF.nCells),Modflow.GWF.zCell(Modflow.GWF.nCells),Modflow.GWF.iLayer(Modflow.GWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'GWF cell coordinate arrays')
        Modflow.GWF.xCell(:)=-999.
        Modflow.GWF.yCell(:)=-999.
        Modflow.GWF.zCell(:)=-999.
        Modflow.GWF.iLayer(:)=-999
        Modflow.GWF.iNode = 0 ! automatic initialization
        
        if(Modflow.NodalControlVolume) then
            do i=1,Modflow.GWF.nElements
                read(Modflow.iGSF,*) i1,Modflow.GWF.nNodesPerCell,(Modflow.GWF.iNode(j,i),j=1,Modflow.GWF.nNodesPerCell)
            end do
            do i=1,Modflow.GWF.nCells
                read(Modflow.iGSF,*) i1,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i)
            end do
        else
            do i=1,Modflow.GWF.nElements
                !read(Modflow.iGSF,*) i1,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i),i2,&
                !    (Modflow.GWF.iNode(j,i),j=1,Modflow.GWF.nNodesPerCell)
                read(Modflow.iGSF,*) i1,Modflow.GWF.xCell(i),Modflow.GWF.yCell(i),Modflow.GWF.zCell(i),Modflow.GWF.iLayer(i),i3,(Modflow.GWF.iNode(j,i),j=1,Modflow.GWF.nNodesPerCell)
            end do
        endif
            
        

	    call freeunit(Modflow.iGSF)
        
        Modflow.GWF.IsDefined=.true.
        allocate(modflow.GWF.Cell_Is(modflow.GWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'GWF Cell_Is array')            
        modflow.GWF.Cell_Is(:)=0
    
        write(TmpSTR,'(i10)') Modflow.GWF.nCells 
        call Msg('Number of Cells: '//trim(TmpSTR))

	    return
    end subroutine Read_GWF_GSF
    
    subroutine Read_CLN_GSF(Modflow)
        implicit none
        
        type (ModflowProject) Modflow
        integer :: i, j
        
        integer :: i1, i2
        real :: r1, r2, r3
        
        ! read initial comment lines beginning with #
        do 
            read(Modflow.iCLN_GSF,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            end if
            backspace(Modflow.iCLN_GSF)
            exit
        end do

        read(Modflow.iCLN_GSF,*) Modflow.CLN.meshtype
        read(Modflow.iCLN_GSF,*) Modflow.CLN.nCells, Modflow.CLN.nLayers, Modflow.CLN.iz, Modflow.CLN.ic
        read(Modflow.iCLN_GSF,*) Modflow.CLN.nNodes
        Modflow.CLN.nElements=Modflow.CLN.nCells

        allocate(Modflow.CLN.x(Modflow.CLN.nNodes),Modflow.CLN.y(Modflow.CLN.nNodes),Modflow.CLN.z(Modflow.CLN.nNodes), stat=ialloc)
        call AllocChk(ialloc,'CLN node coordinate arrays')
        Modflow.CLN.x = 0 ! automatic initialization
        Modflow.CLN.y = 0 ! automatic initialization
        Modflow.CLN.z = 0 ! automatic initialization
        
        read(Modflow.iCLN_GSF,*) (Modflow.CLN.x(i),Modflow.CLN.y(i),Modflow.CLN.z(i),i=1,Modflow.CLN.nNodes)

        ! determine the number of nodes per cell (Modflow.CLN.nNodesPerCell)
        read(Modflow.iCLN_GSF,*) i1,r1,r2,r3,i2,Modflow.CLN.nNodesPerCell
        backspace(Modflow.iCLN_GSF)

        allocate(Modflow.CLN.iNode(Modflow.CLN.nNodesPerCell,Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,'CLN iNode array')
        
        allocate(Modflow.CLN.xCell(Modflow.CLN.nCells),Modflow.CLN.yCell(Modflow.CLN.nCells),Modflow.CLN.zCell(Modflow.CLN.nCells),Modflow.CLN.iLayer(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,'CLN cell coordinate arrays')

        Modflow.CLN.iNode = 0 ! automatic initialization
        do i=1,Modflow.CLN.nCells
            read(Modflow.iCLN_GSF,*) i1,Modflow.CLN.xCell(i),Modflow.CLN.yCell(i),Modflow.CLN.zCell(i),Modflow.CLN.iLayer(i),i2,(Modflow.CLN.iNode(j,i),j=1,Modflow.CLN.nNodesPerCell)
        end do
	    call freeunit(Modflow.iCLN_GSF)
        
        Modflow.CLN.IsDefined=.true.
        allocate(Modflow.CLN.Cell_Is(Modflow.CLN.nCells),stat=ialloc)
        call AllocChk(ialloc,'CLN Cell_Is array')            
        Modflow.CLN.Cell_Is(:)=0

        write(TmpSTR,'(i10)') Modflow.CLN.nCells 
        call Msg('Number of Cells: '//trim(TmpSTR))

        return
    end subroutine Read_CLN_GSF

    subroutine Read_SWF_GSF(Modflow)
        implicit none
        
        type (ModflowProject) Modflow

        integer :: i, j
        
        integer :: i1, i2, i3
        real :: r1, r2, r3
        
        ! read initial comment lines beginning with #
        do 
            read(Modflow.iSWF_GSF,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            end if
            backspace(Modflow.iSWF_GSF)
            exit
        end do

        read(Modflow.iSWF_GSF,'(a)') Modflow.SWF.meshtype
        
        if(Modflow.SWF.meshtype=='UNSTRUCTURED(NODALCONTROLVOLUME)') then
            Modflow.NodalControlVolume=.true.
            read(Modflow.iSWF_GSF,*) Modflow.SWF.nElements, Modflow.SWF.nLayers, Modflow.SWF.iz, Modflow.SWF.ic
            read(Modflow.iSWF_GSF,*) Modflow.SWF.nNodes
            Modflow.SWF.nCells=Modflow.SWF.nNodes
        else
            read(Modflow.iSWF_GSF,*) Modflow.SWF.nElements, Modflow.SWF.nLayers, Modflow.SWF.iz, Modflow.SWF.ic
            read(Modflow.iSWF_GSF,*) Modflow.SWF.nNodes
            Modflow.SWF.nCells=Modflow.SWF.nElements
        end if
       
        
        allocate(Modflow.SWF.x(Modflow.SWF.nNodes),Modflow.SWF.y(Modflow.SWF.nNodes),Modflow.SWF.z(Modflow.SWF.nNodes), stat=ialloc)
        call AllocChk(ialloc,'SWF node coordinate arrays')
        Modflow.SWF.x = 0 ! automatic initialization
        Modflow.SWF.y = 0 ! automatic initialization
        Modflow.SWF.z = 0 ! automatic initialization
        
        read(Modflow.iSWF_GSF,*) (Modflow.SWF.x(i),Modflow.SWF.y(i),Modflow.SWF.z(i),i=1,Modflow.SWF.nNodes)
        
        ! determine the number of nodes per cell (Modflow.SWF.nNodesPerCell)
        if(Modflow.NodalControlVolume) then
            read(Modflow.iSWF_GSF,*) i1,Modflow.SWF.nNodesPerCell
        else
            read(Modflow.iSWF_GSF,*) i1,r1,r2,r3,i2,Modflow.SWF.nNodesPerCell
        endif
        backspace(Modflow.iSWF_GSF)
        allocate(Modflow.SWF.iNode(Modflow.SWF.nNodesPerCell,Modflow.SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,'SWF iNode arrays')
        Modflow.SWF.iNode = 0 ! automatic initialization

        allocate(Modflow.SWF.xCell(Modflow.SWF.nCells),Modflow.SWF.yCell(Modflow.SWF.nCells),Modflow.SWF.zCell(Modflow.SWF.nCells),Modflow.SWF.iLayer(Modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'SWF xyz cell, iLayer arrays')
        Modflow.SWF.xCell(:)=-999.
        Modflow.SWF.yCell(:)=-999.
        Modflow.SWF.zCell(:)=-999.
        Modflow.SWF.iLayer(:)=-999
        
        if(Modflow.NodalControlVolume) then
            do i=1,Modflow.SWF.nElements
                read(Modflow.iSWF_GSF,'(12i10)') i1,Modflow.SWF.nNodesPerCell,(Modflow.SWF.iNode(j,i),j=1,Modflow.SWF.nNodesPerCell)
            end do
            do i=1,Modflow.SWF.nCells
                read(Modflow.iSWF_GSF,'(i10,3(1pg15.5),i10)') i1,Modflow.SWF.xCell(i),Modflow.SWF.yCell(i),Modflow.SWF.zCell(i),Modflow.SWF.iLayer(i)
            end do

        else
            do i=1,Modflow.SWF.nElements
                read(Modflow.iSWF_GSF,*) i1,Modflow.SWF.xCell(i),Modflow.SWF.yCell(i),Modflow.SWF.zCell(i),Modflow.SWF.iLayer(i),i3,(Modflow.SWF.iNode(j,i),j=1,Modflow.SWF.nNodesPerCell)
            end do
        end if
	    call freeunit(Modflow.iSWF_GSF)
        
        Modflow.SWF.IsDefined=.true.

        write(TmpSTR,'(i10)') Modflow.SWF.nCells 
        call Msg('Number of Cells: '//trim(TmpSTR))
        allocate(modflow.SWF.Cell_Is(modflow.SWF.nCells),stat=ialloc)
        call AllocChk(ialloc,'SWF Cell_Is array')            
        modflow.SWF.Cell_Is(:)=0
    
    

	    return
    end subroutine Read_SWF_GSF
   
    !----------------------------------------------------------------------
    subroutine UnitsLength(FnumMUT,Project) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) Project
        
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
            call ErrMsg('Units of length '//trim(value)//' not recognized. Must be feet, meters, or centimeters.')
        end select
        
        write(TmpSTR,'(a)')    TAB//'Units of length:   '//trim(Project.STR_LengthUnit)
        call Msg(TmpSTR)

    end subroutine UnitsLength

    !----------------------------------------------------------------------
    subroutine UnitsTime(FNumMUT,Project) 
        implicit none

        integer :: FNumMUT
        type (ModflowProject) Project
        
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
            call ErrMsg('Units of time '//trim(value)//' not recognized. Must be seconds, minutes, hours, days or years.')
        end select
        
        write(TmpSTR,'(a)')    TAB//'Units of time:   '//trim(Project.STR_TimeUnit)
        call Msg(TmpSTR)

    end subroutine UnitsTime

    
    
    subroutine URWORD(line,icol,istart,istop,ncode,n,r,iout,in)
        !c     ******************************************************************
        !c     routine to extract a word from a line of text, and optionally
        !c     convert the word to a number.
        !c        istart and istop will be returned with the starting and
        !c          ending character positions of the word.
        !c        the last character in the line is set to blank so that if any
        !c          problems occur with finding a word, istart and istop will
        !c          point to this blank character.  thus, a word will always be
        !c          returned unless there is a numeric conversion error.  be sure
        !c          that the last character in line is not an important character
        !c          because it will always be set to blank.
        !c        a word starts with the first character that is not a space or
        !c          comma, and ends when a subsequent character that is a space
        !c          or comma.  note that these parsing rules do not treat two
        !c          commas separated by one or more spaces as a null word.
        !c        for a word that begins with "'", the word starts with the
        !c          character after the quote and ends with the character
        !c          preceding a subsequent quote.  thus, a quoted word can
        !c          include spaces and commas.  the quoted word cannot contain
        !c          a quote character.
        !c        if ncode is 1, the word is converted to upper case.
        !c        if ncode is 2, the word is converted to an integer.
        !c        if ncode is 3, the word is converted to a real number.
        !c        number conversion error is written to unit iout if iout is
        !c          positive; error is written to default output if iout is 0;
        !c          no error message is written if iout is negative.
        !c     ******************************************************************
        !c

        use ifport
        implicit none

        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        CHARACTER*(*) LINE
        CHARACTER*20 STRING
        CHARACTER*30 RW
        CHARACTER*1 TAB

        ! rgm modified ...
        integer :: len
        integer :: linlen
        integer :: in
        integer :: icol
        integer :: iout
        real :: r
        integer :: istart
        integer :: istop
        integer :: n
        integer :: ii, jj, kk
        integer :: ncode
        integer :: l
        integer :: idiff
        

        
        IOUT=FNumEco
        !... to here

        !     ------------------------------------------------------------------
        TAB=CHAR(9)
        !
        !1------Set last char in LINE to blank and set ISTART and ISTOP to point
        !1------to this blank as a default situation when no word is found.  If
        !1------starting location in LINE is out of bounds, do not look for a
        !1------word.
        LINLEN=LEN(LINE)
        LINE(LINLEN:LINLEN)=' '
        ISTART=LINLEN
        ISTOP=LINLEN
        LINLEN=LINLEN-1
        IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
        !
        !2------Find start of word, which is indicated by first character that
        !2------is not a blank, a comma, or a tab.
        DO 10 II=ICOL,LINLEN
        IF(LINE(II:II).NE.' ' .AND. LINE(II:II).NE.','&
        .AND. LINE(II:II).NE.TAB) GO TO 20
        10    CONTINUE
        ICOL=LINLEN+1
        GO TO 100
        !
        !3------Found start of word.  Look for end.
        !3A-----When word is quoted, only a quote can terminate it.
        20    IF(LINE(II:II).EQ.'''') THEN
            II=II+1
            IF(II.LE.LINLEN) THEN
                DO 25 JJ=II,LINLEN
                    IF(LINE(JJ:JJ).EQ.'''') GO TO 40
                25          CONTINUE
            END IF
            !
            !3B-----When word is not quoted, space, comma, or tab will terminate.
        ELSE
            DO 30 JJ=II,LINLEN
                IF(LINE(JJ:JJ).EQ.' ' .OR. LINE(JJ:JJ).EQ.','&
                .OR. LINE(JJ:JJ).EQ.TAB) GO TO 40
            30       CONTINUE
        END IF
        !
        !3C-----End of line without finding end of word; set end of word to
        !3C-----end of line.
        JJ=LINLEN+1
        !
        !4------Found end of word; set JJ to point to last character in WORD and
        !-------set ICOL to point to location for scanning for another word.
        40    ICOL=JJ+1
        JJ=JJ-1
        IF(JJ.LT.II) GO TO 100
        ISTART=II
        ISTOP=JJ
        !
        !5------Convert word to upper case and RETURN if NCODE is 1.
        IF(NCODE.EQ.1) THEN
            IDIFF=ICHAR('a')-ICHAR('A')
            DO 50 KK=ISTART,ISTOP
                IF(LINE(KK:KK).GE.'a' .AND. LINE(KK:KK).LE.'z')&
                        LINE(KK:KK)=CHAR(ICHAR(LINE(KK:KK))-IDIFF)
            50       CONTINUE
            !IF (IOUT.GT.0)  write(iout,*) '$$$ urword string: ',&
            !LINE(ISTART:ISTOP)

            RETURN
        END IF
        !
        !6------Convert word to a number if requested.
        100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
            RW=' '
            L=30-ISTOP+ISTART
            IF(L.LT.1) GO TO 200
            RW(L:30)=LINE(ISTART:ISTOP)
            IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
            !IF (IOUT.GT.0 .and. NCODE.EQ.2 )  write(iout,*)&
                !'$$$ urword integer: ',N
            IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
            !IF (IOUT.GT.0 .and. NCODE.EQ.3 )  write(iout,*)&
                !'$$$ urword real: ',R
        END IF
        RETURN
        !
        !7------Number conversion error.
        200   IF(NCODE.EQ.3) THEN
            STRING= 'A REAL NUMBER'
            L=13
        ELSE
            STRING= 'AN INTEGER'
            L=10
        END IF
        !
        !7A-----If output unit is negative, set last character of string to 'E'.
        IF(IOUT.LT.0) THEN
            N=0
            R=0.
            LINE(LINLEN+1:LINLEN+1)='E'
            RETURN
            !
            !7B-----If output unit is positive; write a message to output unit.
        ELSE IF(IOUT.GT.0) THEN
            IF(IN.GT.0) THEN
                WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
            ELSE
                WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
            END IF
            201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A,&
            '" TO ',A,' IN LINE:',/1X,A)
            202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,&
            '" TO ',A,' IN LINE:',/1X,A)
            !
            !7C-----If output unit is 0; write a message to default output.
        ELSE
            IF(IN.GT.0) THEN
                WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
            ELSE
                WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
            END IF
        END IF
        !
        !7D-----STOP after writing message.
        CALL USTOP(' ')
    end subroutine URWORD
    
    subroutine URWORD8(line,icol,istart,istop,ncode,n,r,iout,in)
        !c     ******************************************************************
        !c     routine to extract a word from a line of text, and optionally
        !c     convert the word to a number.
        !c        istart and istop will be returned with the starting and
        !c          ending character positions of the word.
        !c        the last character in the line is set to blank so that if any
        !c          problems occur with finding a word, istart and istop will
        !c          point to this blank character.  thus, a word will always be
        !c          returned unless there is a numeric conversion error.  be sure
        !c          that the last character in line is not an important character
        !c          because it will always be set to blank.
        !c        a word starts with the first character that is not a space or
        !c          comma, and ends when a subsequent character that is a space
        !c          or comma.  note that these parsing rules do not treat two
        !c          commas separated by one or more spaces as a null word.
        !c        for a word that begins with "'", the word starts with the
        !c          character after the quote and ends with the character
        !c          preceding a subsequent quote.  thus, a quoted word can
        !c          include spaces and commas.  the quoted word cannot contain
        !c          a quote character.
        !c        if ncode is 1, the word is converted to upper case.
        !c        if ncode is 2, the word is converted to an integer.
        !c        if ncode is 3, the word is converted to a real number.
        !c        number conversion error is written to unit iout if iout is
        !c          positive; error is written to default output if iout is 0;
        !c          no error message is written if iout is negative.
        !c     ******************************************************************
        !c
        !c        specifications:
        !c     ------------------------------------------------------------------
        use ifport
        implicit none

    
        character*(*) line
        character*20 string
        character*30 rw
        character*1 tab
        
        ! rgm modified ...
        integer :: len
        integer :: linlen
        integer :: in
        integer :: icol
        integer :: iout
        real*8 :: r
        integer :: istart
        integer :: istop
        integer :: n
        integer :: ii, jj, kk
        integer :: ncode
        integer :: l
        integer :: idiff
        
        !... to here

        
        !c     ------------------------------------------------------------------
        tab=char(9)
        !c
        !c1------set last char in line to blank and set istart and istop to point
        !c1------to this blank as a default situation when no word is found.  if
        !c1------starting location in line is out of bounds, do not look for a
        !c1------word.
        linlen=len(line)
        line(linlen:linlen)=' '
        istart=linlen
        istop=linlen
        linlen=linlen-1
        if(icol.lt.1 .or. icol.gt.linlen) go to 100
        !c
        !c2------find start of word, which is indicated by first character that
        !c2------is not a blank, a comma, or a tab.
        do 10 ii=icol,linlen
            if(line(ii:ii).ne.' ' .and. line(ii:ii).ne.',' .and. line(ii:ii).ne.tab) go to 20
10      continue
        icol=linlen+1
        go to 100
        !c
        !c3------found start of word.  look for end.
        !c3a-----when word is quoted, only a quote can terminate it.
20      if(line(ii:ii).eq.'''') then
            ii=ii+1
            if(ii.le.linlen) then
                do 25 JJ=ii,linlen
                    if(line(JJ:JJ).eq.'''') go to 40
25              continue
            end if
        !c
        !c3b-----when word is not quoted, space, comma, or tab will terminate.
        else
            do 30 JJ=ii,linlen
                if(line(JJ:JJ).eq.' ' .or. line(JJ:JJ).eq.',' .or. line(JJ:JJ).eq.tab) go to 40
30          continue
        end if
        !c
        !c3c-----end of line without finding end of word; set end of word to
        !c3c-----end of line.
        JJ=linlen+1
        !c
        !c4------found end of word; set JJ to point to last character in word and
        !c-------set icol to point to location for scanning for another word.
40      icol=JJ+1
        JJ=JJ-1
        if(JJ.lt.ii) go to 100
        istart=ii
        istop=JJ
        !c
        !c5------convert word to upper case and return if ncode is 1.
         if(ncode.eq.1) then
            idiff=ichar('a')-ichar('a')
            do 50 KK=istart,istop
                if(line(KK:KK).ge.'a' .and. line(KK:KK).le.'z') line(KK:KK)=char(ichar(line(KK:KK))-idiff)
50          continue
            return
         end if
        !c
        !c6------convert word to a number if requested.
100     if(ncode.eq.2 .or. ncode.eq.3) then
            rw=' '
            l=30-istop+istart
            if(l.lt.1) go to 200
            rw(l:30)=line(istart:istop)
            if(ncode.eq.2) read(rw,'(i30)',err=200) n
            IF (IOUT.GT.0 .and. NCODE.EQ.2 )  write(iout,*)&
                '$$$ urword integer: ',N
            if(ncode.eq.3) read(rw,'(f30.0)',err=200) r
            IF (IOUT.GT.0 .and. NCODE.EQ.3 )  write(iout,*)&
                '$$$ urword real: ',R
        end if
        return
        !c
        !c7------number conversion error.
200     if(ncode.eq.3) then
            string= 'a real number'
            l=13
        else
            string= 'an integer'
            l=10
        end if
        !c
        !c7a-----if output unit is negative, set last character of string to 'e'.
        if(iout.lt.0) then
            n=0
            r=0.
            line(linlen+1:linlen+1)='e'
            return
        !c
        !c7b-----if output unit is positive; write a message to output unit.
        else if(iout.gt.0) then
            if(in.gt.0) then
                write(iout,201) in,line(istart:istop),string(1:l),line
            else
                write(iout,202) line(istart:istop),string(1:l),line
            end if
201         format(1x,/1x,'file unit ',i4,' : error converting "',a,'" to ',a,' in line:',/1x,a)
202         format(1x,/1x,'keyboard input : error converting "',a,'" to ',a,' in line:',/1x,a)
        !c
        !c7c-----if output unit is 0; write a message to default output.
        else
            if(in.gt.0) then
                write(*,201) in,line(istart:istop),string(1:l),line
            else
                write(*,202) line(istart:istop),string(1:l),line
            end if
        end if
        !c
        !c7d-----stop after writing message.
        call ustop(' ')
    end subroutine URWORD8

    subroutine ustop(stopmess)
    !c     ******************************************************************
    !c     stop program, with option to print message before stopping
    !c     ******************************************************************
    !c        specifications:
    !c     ------------------------------------------------------------------
        character stopmess*(*)
    !c     ------------------------------------------------------------------
    

    if (stopmess.ne.' ') then
        write(*,10) stopmess
10      format(1x,a)
    end if
    stop
    
    end subroutine ustop 
    
    SUBROUTINE UPCASE(WORD)
        !     ******************************************************************
        !     CONVERT A CHARACTER STRING TO ALL UPPER CASE
        !     ******************************************************************
        !       SPECIFICATIONS:
        !     ------------------------------------------------------------------
              CHARACTER WORD*(*)
        !
              integer :: l, len, idiff, k
        !1------Compute the difference between lowercase and uppercase.
              L = LEN(WORD)
              IDIFF=ICHAR('a')-ICHAR('A')
        !
        !2------Loop through the string and convert any lowercase characters.
              DO 10 K=1,L
              IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z')&
                WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
        10    CONTINUE
        !
        !3------return.
              RETURN
    END SUBROUTINE UPCASE

    SUBROUTINE UINSRP(I,IN,IOUT,IP,ITERP)
        !     ******************************************************************
        !     Read and store one instance name.
        !     I is the instance number, and IP is the parameter number.
        !     ******************************************************************
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              CHARACTER*10 CTMP1,CTMP2
              CHARACTER*400 LINE
              
              integer :: ip, iout, iterp, i, in, ipl4, iloc, lloc, istart
              real :: r
              integer :: n, istop, j
        !     ------------------------------------------------------------------
        !
        !1------COMPUTE LOCATION OF NAME IN INAME, AND READ LINE CONTAINING
        !1------INSTANCE NAME.
              IPL4 = IPLOC(4,IP)
              ILOC = IPL4+I-1
              READ(IN,1000) LINE
         1000 FORMAT(A)
        !
        !2------GET INSTANCE NAME AND STORE IN INAME.
              LLOC = 1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
              INAME(ILOC) = LINE(ISTART:ISTOP)
              CTMP1 = LINE(ISTART:ISTOP)
              CALL UPCASE(CTMP1)
        !
        !3------WRITE NAME UNLESS THIS IS NOT THE FIRST TIME THE SIMULATION
        !3------HAS BEEN RUN.
              IF(ITERP.EQ.1) WRITE(IOUT,1010)INAME(ILOC)
         1010 FORMAT(/,3X,'INSTANCE:  ',A)
        !
        !4------CHECK FOR DUPLICATE INSTANCE NAME IF THIS IS NOT THE FIRST
        !4------INSTANCE.
              IF (I.GT.1) THEN
                DO 10 J=IPL4,IPL4+I-2
                  CTMP2 = INAME(J)
                  CALL UPCASE(CTMP2)
                  IF (CTMP1.EQ.CTMP2) THEN
                    WRITE(IOUT,1020)INAME(J)
         1020       FORMAT(/,1X,'*** ERROR: "',A,&
             '" IS A DUPLICATE INSTANCE NAME FOR THIS PARAMETER',/,&
             ' -- STOP EXECUTION (UINSRP)')
                    CALL USTOP(' ')
                  end if
           10   CONTINUE
              end if
        !
        !5------RETURN.
      RETURN
    END SUBROUTINE UINSRP

    SUBROUTINE URDCOM(IN,IOUT,LINE)
    !C     ******************************************************************
    !C     READ COMMENTS FROM A FILE AND PRINT THEM.  RETURN THE FIRST LINE
    !C     THAT IS NOT A COMMENT
    !C     ******************************************************************
    !C
    !C        SPECIFICATIONS:
    !C     ------------------------------------------------------------------
    
    implicit none
    
    CHARACTER*(*) LINE
    
    integer :: IOUT
    integer :: IN
    integer :: LEN
    integer :: L
    integer :: II

    IOUT=FNumEco

    !C     ------------------------------------------------------------------
    !C
    !C1------Read a line
10  READ(IN,'(A)') LINE
    !C
    !C2------If the line does not start with "#", return.
    IF(LINE(1:1).NE.'#') then
        !IF (IOUT.GT.0)  write(iout,*) '$$$ urdcom unit#: ',IN
        if(IN.eq.22) then
            continue
        end if 
        RETURN
    end if
    !C
    !C3------Find the last non-blank character.
    L=LEN(LINE)
    DO 20 II=L,1,-1
        IF(LINE(II:II).NE.' ') GO TO 30
20  CONTINUE
    !C
    !C4------Print the line up to the last non-blank character if IOUT>0.
30  IF (IOUT.GT.0) WRITE(IOUT,'(1X,A)') LINE(1:II)
    GO TO 10
 
    END SUBROUTINE URDCOM

    SUBROUTINE U1DREL(A,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*24 ANAME
      real :: A(nJJ)
      real :: PERTIM,TOTIM
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      INCLUDE 'openspec.inc'
      
      integer :: in, KK, iout, jj
      integer :: nunopn
      DATA NUNOPN/99/
      integer :: iclose, ifree,icol
      real :: R
      integer :: istart, istop,n,locat
      real :: cnstnt
      integer :: iprn, kper, nstrt, ilay, kstp, nndlay, nJJ
      
      IOUT=FNumEco

!     ------------------------------------------------------------------

      !write(iout,*) '$$$ U1DREL aname: ',trim(ANAME)
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GE.0) GO TO 71
!-------LOCAT <0 READ BINARY FILE
!4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(KK.GT.0) THEN
           WRITE(IOUT,201) ANAME,KK,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1x,'READING BINARY ON UNIT ',I4)
        ELSE IF(KK.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NSTRT,NNDLAY,ILAY
        READ(LOCAT) (A(JJ),JJ=1,nJJ)
        RETURN

71    IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 A(JJ)=CNSTNT
      IF(KK.GT.0.OR.KK.LT.0) WRITE(IOUT,2) ANAME,CNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      IF(KK.EQ.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (A(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 A(JJ)=A(JJ)*CNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(JJ),JJ=1,nJJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(JJ),JJ=1,nJJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
    END SUBROUTINE U1DREL
    
    
    SUBROUTINE U1DREL8(A,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*24 ANAME
      DOUBLE PRECISION :: A(nJJ)
      DOUBLE PRECISION :: PERTIM,TOTIM
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      INCLUDE 'openspec.inc'
      
      integer :: in, KK, iout, jj
      integer :: nunopn
      DATA NUNOPN/99/
      integer :: iclose, ifree,icol
      real :: R
      integer :: istart, istop,n,locat
      real :: cnstnt
      integer :: iprn, kper, nstrt, ilay, kstp, nndlay, nJJ
      
      IOUT=FNumEco

!     ------------------------------------------------------------------

      !write(iout,*) '$$$ U1DREL8 aname: ',trim(ANAME)
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GE.0) GO TO 71
!-------LOCAT <0 READ BINARY FILE
!4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(KK.GT.0) THEN
           WRITE(IOUT,201) ANAME,KK,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1x,'READING BINARY ON UNIT ',I4)
        ELSE IF(KK.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NSTRT,NNDLAY,ILAY
        READ(LOCAT) (A(JJ),JJ=1,nJJ)
        RETURN

71    IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 A(JJ)=CNSTNT
      IF(KK.GT.0.OR.KK.LT.0) WRITE(IOUT,2) ANAME,CNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      IF(KK.EQ.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (A(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 A(JJ)=A(JJ)*CNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(JJ),JJ=1,nJJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(JJ),JJ=1,nJJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
    END SUBROUTINE U1DREL8
    
    
    SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
        !     ******************************************************************
        !     ROUTINE TO INPUT 2-D REAL DATA MATRICES
        !       A IS ARRAY TO INPUT
        !       ANAME IS 24 CHARACTER DESCRIPTION OF A
        !       II IS NO. OF ROWS
        !       JJ IS NO. OF COLS
        !       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
        !              IF K=0, NO LAYER IS PRINTED
        !              IF K<0, CROSS SECTION IS PRINTED)
        !       IN IS INPUT UNIT
        !       IOUT IS OUTPUT UNIT
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              CHARACTER*24 ANAME
              DIMENSION A(JJ,II)
              CHARACTER*20 FMTIN
              CHARACTER*400 CNTRL
              CHARACTER*16 TEXT
              CHARACTER*400 FNAME
              REAL PERTIMRD,TOTIMRD
              
              integer :: NUNOPN
              DATA NUNOPN/99/
              integer :: in, i,j,k, iout, iclose, ifree, icol, n, istop, istart, locat, iprn, jj, ii
              real :: a, r, cnstnt
              integer :: kstp, ilay, kper
              
              INCLUDE 'openspec.inc'
        !     ------------------------------------------------------------------
              !write(iout,*) '$$$ U2DREL aname: ',trim(ANAME)
        !
        !1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
              READ(IN,'(A)') CNTRL
        !
        !2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
        !2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
              ICLOSE=0
              IFREE=1
              ICOL=1
              CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
              IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
                 LOCAT=0
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
                 LOCAT=IN
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
                 FNAME=CNTRL(ISTART:ISTOP)
                 LOCAT=NUNOPN
                 WRITE(IOUT,15) LOCAT,FNAME
           15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
                 ICLOSE=1
              ELSE
        !
        !2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
        !2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
                 IFREE=0
                 READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
            1    FORMAT(I10,F10.0,A20,I10)
              END IF
        !
        !3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
              IF(IFREE.NE.0) THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
                 IF(LOCAT.NE.0) THEN
                    CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
                    FMTIN=CNTRL(ISTART:ISTOP)
                    IF(ICLOSE.NE.0) THEN
                       IF(FMTIN.EQ.'(BINARY)') THEN
                          OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,&
                              ACTION=ACTION(1))
                       ELSE
                          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
                       END IF
                    END IF
                    IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
                    CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
                 END IF
              END IF
        !
        !4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
              IF(LOCAT.EQ.0) THEN
        !
        !4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
                DO 80 I=1,II
                DO 80 J=1,JJ
           80   A(J,I)=CNSTNT
                IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
            2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
                IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
            3   FORMAT(1X,/1X,A,' =',1P,G14.6)
                RETURN
              ELSE IF(LOCAT.GT.0) THEN
        !
        !4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
                IF(K.GT.0) THEN
                   WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
           94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                ELSE IF(K.EQ.0) THEN
                   WRITE(IOUT,95) ANAME,LOCAT,FMTIN
           95      FORMAT(1X,///11X,A,/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                ELSE
                   WRITE(IOUT,96) ANAME,LOCAT,FMTIN
           96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                END IF
                DO 100 I=1,II
                IF(FMTIN.EQ.'(FREE)') THEN
                   READ(LOCAT,*) (A(J,I),J=1,JJ)
                ELSE
                   READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
                END IF
          100   CONTINUE
              ELSE
        !
        !4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
                LOCAT=-LOCAT
                IF(K.GT.0) THEN
                   WRITE(IOUT,201) ANAME,K,LOCAT
          201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
                   1X,'READING BINARY ON UNIT ',I4)
                ELSE IF(K.EQ.0) THEN
                   WRITE(IOUT,202) ANAME,LOCAT
          202      FORMAT(1X,///1X,A,/&
                   1X,'READING BINARY ON UNIT ',I4)
                ELSE
                   WRITE(IOUT,203) ANAME,LOCAT
          203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
                   1X,'READING BINARY ON UNIT ',I4)
                END IF
                READ(LOCAT) KSTP,KPER,PERTIMRD,TOTIMRD,TEXT,NCOL,NROW,ILAY
                READ(LOCAT) A
              END IF
        !
        !5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
              IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
              ZERO=0.
              IF(CNSTNT.EQ.ZERO) GO TO 320
              DO 310 I=1,II
              DO 310 J=1,JJ
              A(J,I)=A(J,I)*CNSTNT
          310 CONTINUE
        !
        !6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
          320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
        !
        !7------RETURN
              RETURN
        !
        !8------CONTROL RECORD ERROR.
          500 IF(K.GT.0) THEN
                 WRITE(IOUT,501) ANAME,K
          501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,&
                  ' FOR LAYER',I4,':')
              ELSE
                 WRITE(IOUT,502) ANAME
          502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
              END IF
              WRITE(IOUT,'(1X,A)') CNTRL
              CALL USTOP(' ')
      END SUBROUTINE U2DREL


    SUBROUTINE U1DINT(IA,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION IA(nJJ)
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: iout,ia,KK,in
      integer :: NUNOPN
      DATA NUNOPN/99/
      integer :: jj, iclose, ifree, icol, istart
      real :: R
      integer :: n
      integer :: istop, locat, iprn, icnstnt, nJJ
        
      IOUT=FNumEco
      

!     ------------------------------------------------------------------
      !write(iout,*) '$$$ U1DINT aname: ',trim(ANAME)
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,ICNSTNT,FMTIN,IPRN
    1    FORMAT(I10,I10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICNSTNT,R,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO ICNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 IA(JJ)=ICNSTNT
      IF(KK.GT.0) THEN
        WRITE(IOUT,2) ANAME,ICNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,I10,' FOR LAYER',I4)
      ELSE
        WRITE(IOUT,3) ANAME,ICNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,I10)
      end if
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (IA(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (IA(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF ICNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICNSTNT.
      ZERO=0.
      IF(ICNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 IA(JJ)=IA(JJ)*ICNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (IA(JJ),JJ=1,nJJ)
1001     FORMAT(20(1X,I9))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (IA(JJ),JJ=1,nJJ)
1002     FORMAT(8(1X,I9))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE U1DINT

      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
!       IA IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF IA
!       II IS NO. OF ROWS
!       JJ IS NO. OF COLS
!       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
!              IF K=0, NO LAYER IS PRINTED
!              IF K<0, CROSS SECTION IS PRINTED)
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: iout, k, in, ia, ii, iclose, ifree, icol, istart
      real :: r
      integer :: n, istop, locat, iconst, iprn, i, j, jj
!     ------------------------------------------------------------------
      !write(iout,*) '$$$ U2DINT aname: ',trim(ANAME)
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=600) LOCAT,ICONST,FMTIN,IPRN
    1    FORMAT(I10,I10,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,&
                      ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!
!4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   IA(J,I)=ICONST
        IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
   83   FORMAT(1X,/1X,A,' =',I15)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (IA(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (IA(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!
!4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///11X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT)
        READ(LOCAT) IA
      END IF
!
!5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
!
!6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0) RETURN
!
!7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
!
!8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
!
!----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I2):/(5X,60(1X,I2)))
      GO TO 510
!
!----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
!
!----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
!
!----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
!
!----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
!
!----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
!
!----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
!
!----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,10(1X,I4)))
      GO TO 510
!
!----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
!
  510 CONTINUE
!
!9------RETURN
      RETURN
!
!10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(IOUT,601) ANAME,K
  601    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,&
          ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,602) ANAME
  602    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE U2DINT

    end module MUSG

