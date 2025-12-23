module MUSG_Core
    !### Core MODFLOW-USG data structures
    ! Defines fundamental types for MODFLOW-USG processing: cell, ModflowDomain, ModflowProject
    
    use KindParameters
    use NumericalMesh
    use GeneralRoutines, only: MAX_LBL, MAX_OBS, MAX_STR
    
    implicit none
    private
    
    public :: cell, ModflowDomain, ModflowProject
    public :: NodalControlVolume
    
    ! Domain type constants (private to avoid conflict with ModflowProject file unit fields iCLN, iSWF)
    ! These are used internally but not exported to avoid naming conflicts
    integer(i4), parameter, private :: DOMAIN_GWF=1
    integer(i4), parameter, private :: DOMAIN_SWF=2
    integer(i4), parameter, private :: DOMAIN_CLN=3

    ! By default, 2D finite-elements in template mesh are used to define control volumes
    logical :: NodalControlVolume=.false.

    !----------------------------------------------------------------------
    type cell 
        real(dp) :: x
        real(dp) :: y
        real(dp) :: z
        real(dp) :: Area   ! true area of the cell (currently not used)
        real(dp) :: xyArea   ! area of the cell in xy plane
        real(dp) :: Top    ! top elevation of the cell 
        real(dp) :: Bottom ! bottom elevation of the cell
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
        integer(i4) :: idZone
        integer(i4) :: iLayer ! layer number for extruded mesh
        
        ! General cell properties used by all domain types
        real(dp) :: StartingHeads           ! STRT in modflow i.e. initial heads
        
        ! GWF cell properties 
        real(sp) :: Kh
        real(sp) :: Kv
        real(sp) :: Ss
        real(sp) :: Sy
        real(sp) :: Alpha
        real(sp) :: Beta
        real(sp) :: Sr
        real(sp) :: Brooks
        
        ! SWF cell properties 
        real(sp) :: Sgcl                    ! SWF-GWF connection length
        real(dp) :: CriticalDepthLength     ! SWBC assigned critical depth boundary cell length value

        
        ! GWF cell properties 
        real(dp) :: Length          
        real(dp) :: LowestElevation
        real(dp) :: SlopeAngle
    end type cell 

    !----------------------------------------------------------------------
    type, extends(mesh) :: ModflowDomain
        ! Common to all types of domains: GWF, CLN, SWF, ...
        
        integer(i4) :: nCells ! number of cells in domain
        type(cell), allocatable :: cell(:) ! array of cells
        integer(i4) :: nNodesPerCell ! number of nodes in cell
        
        real(dp), allocatable :: ConnectionLength(:,:)    ! variable named CLN in modflow, not to be confused with CLN (Connected Linear Network)
        real(dp), allocatable :: PerpendicularArea(:,:)   ! FAHL in modflow

        
        logical :: IsDefined=.false.      ! this type of domain has been defined 
        character(128) :: MeshType      ! structured or unstructured?
        
        integer(i4) :: iz      ! is 1 if the elevations of node and mesh elements vertices are supplied; 0 otherwise
        integer(i4) :: ic      ! is 1 if the cell specifications associated with each node are supplied; 0 otherwise

        integer(i4) :: NCLNGWC      ! # of CLN to GWF connections
        integer(i4) :: NCONDUITYP   ! number of circular CLN's
        integer(i4) :: NRECTYP      ! number of rectangular CLN's
        
        integer(i4) :: nCHDCells=0        
        real(dp), allocatable :: ConstantHead(:)  ! CHD assigned head value

        real(dp), allocatable :: Recharge(:)  ! RCH assigned recharge value
        integer(i4) :: nRCHoption  ! RCH option (nrchop in Modflow)
        
        integer(i4) :: nDRNCells=0        
        real(dp), allocatable :: DrainElevation(:)  ! DRN assigned Drain Elevation value
        real(dp), allocatable :: DrainConductance(:)  ! DRN assigned Drain Conductance value

        integer(i4) :: nWELCells=0        
        real(dp), allocatable :: PumpingRate(:)  ! WEL assigned Pumping Rate value

        ! SWF boundary conditions
        integer(i4) :: nSWBCCells=0        
        real(dp), allocatable :: CriticalDepth(:)  ! SWBC assigned critical depth boundary cell value

        integer(i4), allocatable :: ibound(:)
        integer(i4), allocatable :: laybcd(:)  ! size nLayers, non-zero value indicates layer has a quasi-3D confining bed below
        integer(i4) :: nodelay    ! for now assume a constant for stacked mesh with no vertical refinement

        integer(i4), allocatable :: LayTyp(:)  ! size nLayers, layer type
        integer(i4), allocatable :: LayAvg(:)  ! size nLayers, layer type
        real(dp), allocatable :: chani(:)  ! size nLayers, layer type
        integer(i4), allocatable :: layvka(:)  ! size nLayers, layer type
        integer(i4), allocatable :: laywet(:)  ! size nLayers, layer type

        real(sp), allocatable :: hnew(:)  ! initial head

        ! .HDS file
        character(128) :: FNameHDS
        integer(i4) :: iHDS
        real(sp), allocatable :: Head(:,:)

        ! .DDN file
        character(128) :: FNameDDN
        integer(i4) :: iDDN
        real(sp), allocatable :: Drawdown(:,:)

        ! .CBB file
        integer(i4) :: nComp
        character(128) :: FNameCBB
        integer(i4) :: iCBB
        real(sp), allocatable :: Cbb_STORAGE(:,:)
        real(sp), allocatable :: Cbb_CONSTANT_HEAD(:,:)
        real(sp), allocatable :: Cbb_RECHARGE(:,:)
        real(sp), allocatable :: Cbb_WELLS(:,:)
        real(sp), allocatable :: Cbb_DRAINS(:,:)
        real(sp), allocatable :: Cbb_CLN(:,:)
        real(sp), allocatable :: Cbb_SWF(:,:)
        real(sp), allocatable :: Cbb_FLOW_FACE(:,:)
        real(sp), allocatable :: Cbb_GWF(:,:)
        real(sp), allocatable :: Cbb_SWBC(:,:)

        real(sp), allocatable :: laycbd(:)

        ! CLN properties (zoned)
        integer(i4), allocatable    :: Geometry(:)           ! circular or rectangular
        integer(i4), allocatable    :: Direction(:)          ! vertical, horizontal or angled
        real(sp), allocatable       :: CircularRadius(:)    ! dimension of CLN
        real(sp), allocatable       :: RectangularWidth(:)    ! dimension of CLN
        real(sp), allocatable       :: RectangularHeight(:)    ! dimension of CLN
        real(sp), allocatable       :: LongitudinalK(:)    ! dimension of CLN

        ! GWF cell properties (zoned)
        integer(i4), allocatable :: idMaterial(:)   ! material ID for each cell
        integer(i4), allocatable :: idSWFMaterial(:)   ! SWF material ID for each cell
        integer(i4), allocatable :: idCLNMaterial(:)   ! CLN material ID for each cell
        
        integer(i4), allocatable    :: FlowTreatment(:)       ! confined/unconfined, laminar/turbulent etc

        ! SWF properties (zoned)
        real(sp), allocatable :: Manning(:)   ! Manning's coefficient of friction
        real(sp), allocatable :: DepressionStorageHeight(:)
        real(sp), allocatable :: ObstructionStorageHeight(:)
        real(sp), allocatable :: H1DepthForSmoothing(:)   ! SWF depth smoothing parameter
        real(sp), allocatable :: H2DepthForSmoothing(:)   ! SWF depth smoothing parameter
        
        ! Observation Points
        ! .OBS file
        character(128) :: FNameOBS
        integer(i4) :: iOBS

        integer(i4) :: nObsPnt=0
        character(MAX_LBL) :: ObsPntName(MAX_OBS)
        integer(i4) :: ObsPntCell(MAX_OBS)
            
    end type ModflowDomain

    !----------------------------------------------------------------------
    type ModflowProject
 
        type(mesh) TMPLT
        type(ModflowDomain) GWF
        type(ModflowDomain) CLN
        type(ModflowDomain) SWF
        
        character(128) :: MUTPrefix
        character(128) :: Prefix='Modflow'
        
        
        ! By default, RICHARDS equation for variably-saturated flow is used
        logical :: SaturatedFlow=.false.
        
        logical :: TagFiles
        logical :: GenOCFile

        ! GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameGSF
        integer(i4) :: iGSF
        
        ! CLN_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameCLN_GSF
        integer(i4) :: iCLN_GSF
        
        ! SWF_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameSWF_GSF
        integer(i4) :: iSWF_GSF
        
        ! NAM file
        character(128) :: FNameNAM
        integer(i4) :: iNAM
        
        ! DISU file
        character(128) :: FNameDISU
        integer(i4) :: iDISU

        ! LIST file
        character(128) :: FNameLIST
        integer(i4) :: iLIST
        
        ! Units
        character(MAX_LBL) :: STR_TimeUnit
        integer(i4) :: TimeUnits=1    ! default 1 is seconds
        character(MAX_LBL) :: STR_LengthUnit
        integer(i4) :: LengthUnits=2   ! default 2 is meters
        
        ! BAS6 file
        character(128) :: FNameBAS6
        integer(i4) :: iBAS6
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
        integer(i4) :: ixsec = 0    
        integer(i4) :: ichflg = 0   
        integer(i4) :: ifrefm = 0   
        integer(i4) :: iprtim = 0   
        integer(i4) :: iunstr = 0   
        integer(i4) :: iprconn = 0  
        integer(i4) :: ifrcnvg = 0 
        integer(i4) :: iunsat=1
        integer(i4) :: idpin = 0    
        integer(i4) :: idpout = 0   
        integer(i4) :: ihmsim = 0   
        integer(i4) :: iuihm = 0    
        integer(i4) :: isyall = 0

        ! SMS file
        character(128) :: FNameSMS
        integer(i4) :: iSMS

        ! OC file
        character(128) :: FNameOC
        integer(i4) :: iOC
        integer(i4) :: ntime = 0
        real(sp), allocatable :: timot(:)
        real(dp), allocatable :: OutputTimes(:)
        integer(i4) :: nOutputTimes
        
        !Stress Periods
        integer(i4) :: nPeriods = 0
        real(sp), allocatable :: StressPeriodDuration(:)
        integer(i4), allocatable :: StressPeriodnTsteps(:)
        real(sp), allocatable :: StressPeriodnTstepMult(:)
        character(2), allocatable :: StressPeriodType(:)
        ! Stress period defaults
        real(sp) :: StressPeriodDeltat=1.000000e-03
        real(sp) :: StressPeriodTminat=1.000000e-05
        real(sp) :: StressPeriodTmaxat=60.0d0
        real(sp) :: StressPeriodTadjat=1.100000e+00
        real(sp) :: StressPeriodTcutat=2.000000e+00        
        
        ! LPF file
        character(128) :: FNameLPF
        integer(i4) :: iLPF

        ! RCH file
        character(128) :: FNameRCH
        integer(i4) :: iRCH
        
        !RST file for transient recharge
        character(128) :: FNameRTS
        integer(i4) :: iRTS
        
        
        ! RIV file
        character(128) :: FNameRIV
        integer(i4) :: iRIV
        
        ! WEL file
        character(128) :: FNameWEL
        integer(i4) :: iWEL
        
        ! CHD file
        character(128) :: FNameCHD
        integer(i4) :: iCHD

        ! EVT file
        character(128) :: FNameEVT
        integer(i4) :: iEVT
        
        ! DRN file
        character(128) :: FNameDRN
        integer(i4) :: iDRN

        ! CLN file
        character(128) :: FNameCLN
        integer(i4) :: iCLN

        ! SWF file
        character(128) :: FNameSWF
        integer(i4) :: iSWF

        ! SWBC file
        character(128) :: FNameSWBC
        integer(i4) :: iSWBC

        ! STO file
        character(128) :: FNameSTO
        integer(i4) :: iSTO

        ! UZF file
        character(128) :: FNameUZF
        integer(i4) :: iUZF

        ! SFR file
        character(128) :: FNameSFR
        integer(i4) :: iSFR

        ! LAK file
        character(128) :: FNameLAK
        integer(i4) :: iLAK

        ! MNW2 file
        character(128) :: FNameMNW2
        integer(i4) :: iMNW2

        ! HOB file
        character(128) :: FNameHOB
        integer(i4) :: iHOB

        ! OBS file
        character(128) :: FNameOBS
        integer(i4) :: iOBS

        ! OBPT file (Observation Points file)
        character(128) :: FNameOBPT
        integer(i4) :: iOBPT

        ! BCF6 file (Block-Centered Flow package)
        integer(i4) :: iBCF6

        ! EVS file (Evapotranspiration Segments)
        integer(i4) :: iEVS

        ! GHB file (General Head Boundary)
        integer(i4) :: iGHB

        ! TIB file
        integer(i4) :: iTIB

        ! DPF file
        integer(i4) :: iDPF

        ! PCB file
        integer(i4) :: iPCB

        ! BCT file
        integer(i4) :: iBCT

        ! FHB file (Flow and Head Boundary)
        integer(i4) :: iFHB

        ! RES file (Reservoir)
        integer(i4) :: iRES

        ! STR file (Stream)
        integer(i4) :: iSTR

        ! IBS file (Interbed Storage)
        integer(i4) :: iIBS

        ! HFB6 file (Horizontal Flow Barrier)
        integer(i4) :: iHFB6

        ! DIS file (Discretization)
        integer(i4) :: iDIS

        ! PVAL file (Parameter Values)
        integer(i4) :: iPVAL

        ! SGB file
        integer(i4) :: iSGB

        ! DPT file
        integer(i4) :: iDPT

        ! ZONE file
        integer(i4) :: iZONE

        ! MULT file (Multiplier)
        integer(i4) :: iMULT

        ! DROB file (Drain Observation)
        integer(i4) :: iDROB

        ! RVOB file (River Observation)
        integer(i4) :: iRVOB

        ! GBOB file (General Head Boundary Observation)
        integer(i4) :: iGBOB

        ! GNC file (Ghost Node Correction)
        integer(i4) :: iGNC

        ! DDF file
        integer(i4) :: iDDF

        ! CHOB file (Constant Head Observation)
        integer(i4) :: iCHOB

        ! ETS file (Evapotranspiration Segments)
        integer(i4) :: iETS

        ! DRT file (Drain Return)
        integer(i4) :: iDRT

        ! QRT file
        integer(i4) :: iQRT

        ! GMG file (Geometric Multigrid Solver)
        integer(i4) :: iGMG

        ! hyd file
        integer(i4) :: ihyd

        ! MDT file
        integer(i4) :: iMDT

        ! GAGE file
        integer(i4) :: iGAGE

        ! LVDA file
        integer(i4) :: iLVDA

        ! SYF file
        integer(i4) :: iSYF

        ! LMT6 file (Link-MT3D)
        integer(i4) :: ilmt6

        ! MNW1 file (Multi-Node Well version 1)
        integer(i4) :: iMNW1

        ! KDEP file
        integer(i4) :: iKDEP

        ! SUB file (Subsidence)
        integer(i4) :: iSUB

        ! UZF file (Unsaturated Zone Flow) - already defined above, but keeping for consistency

        ! GWM file (Groundwater Management)
        integer(i4) :: iGWM

        ! SWT file (Subsidence and Aquifer-System Compaction Package for Water-Table Aquifers)
        integer(i4) :: iSWT

        ! PATH file
        integer(i4) :: iPATH

        ! PTH file
        integer(i4) :: iPTH

        ! TVM file
        integer(i4) :: iTVM

        ! SWF file (Surface Water Flow) - already defined above, but keeping for consistency

        ! SWBC file (Surface Water Boundary Condition) - already defined above, but keeping for consistency

        ! STRT file
        character(128) :: FNameSTRT
        integer(i4) :: iSTRT

        ! STRT file
        character(128) :: FNameSTRT_SWF
        integer(i4) :: iSTRT_SWF

        ! STRT file
        character(128) :: FNameSTRT_CLN
        integer(i4) :: iSTRT_CLN

        ! Scan file
        integer(i4) :: nDim=10000
        integer(i4) :: nKeyWord
        character(MAX_STR), allocatable :: KeyWord(:) ! read buffer for location data
        character(128) :: FNameSCAN
        integer(i4) :: iSCAN

    end type ModflowProject

end module MUSG_Core

