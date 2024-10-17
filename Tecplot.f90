module Tecplot !
    use iso_c_binding
    use GeneralRoutines
    use fem
    implicit none

    include "tecio.f90"
    
    integer, parameter :: MAXMEDIA=10
    integer, parameter :: MAXVAR=50
    
    character(256) :: Tecplot_CMD  
    character(MAX_INST) :: Tecplot_ReadWrite_szplt_CMD="read write szplt"
    character(MAX_INST) :: Tecplot_hsplot2012_CMD="hsplot 2012"
    character(MAX_INST) :: TecplotTimeOffset_CMD="tecplot time offset"
    character(MAX_INST) :: Tecplot_ASCIIMode_CMD="ascii mode"
    character(MAX_INST) :: Tecplot_BinaryPLTMode_CMD="binary plt mode"
    character(MAX_INST) :: Tecplot_End_CMD=	'end tecplot'
    
    character(MAX_LBL) :: TPrefix
    
    logical :: BINARYMode=.true.
   
    ! szplt declarations
    integer lastArgNum
    character(256) programName
    character(256) inputFileName
    character(256) outputFileName
    character(256) dataSetTitle, zoneTitle
    character(256) name, val
    character(256) macroFunctionCmd
    character(256) textString, typeface, fontName, testFontName
    character(1024) varNames
    character(1024) labelSet
    character, pointer :: stringPtr(:)
    integer nameLen, strLen
    integer(c_int8_t), allocatable :: int8Values(:)
    integer(c_int16_t), allocatable :: int16Values(:)
    integer(c_int32_t) :: numVars, var
    integer(c_int32_t) :: fileType
    integer(c_int32_t) :: varType
    integer(c_int32_t) :: fileFormat = 1 ! .szplt
    integer(c_int32_t) :: outputDebugInfo = 1
    integer(c_int32_t) :: inputZone, numZones, outputZone
    integer(c_int32_t) :: zoneType
    integer(c_int32_t) :: strandID, parentZone, faceNeighborMode
    integer(c_int32_t) :: shareConnectivityFromZone
    integer(c_int32_t) :: zero = 0
    integer(c_int32_t) :: is64Bit
    integer(c_int32_t) :: numItems, whichItem
    integer(c_int32_t) :: numSets
    integer(c_int32_t) :: numGeoms, geom, geomType, clipping
    integer(c_int32_t) :: color, isFilled, fillColor, coordMode, &
                            linePattern
    integer(c_int32_t) :: attachZone, isAttached, scope, &
                            arrowheadAttachment
    integer(c_int32_t) :: arrowheadStyle, numSegments, segment, ind
    integer(c_int32_t) :: arrayLength
    integer(c_int32_t) :: numEllipsePoints = 2
    integer(c_int32_t) :: numTexts, text
    integer(c_int32_t) :: boxColor, boxFillColor, boxType, anchor
    integer(c_int32_t) :: isBold, isItalic, sizeUnits, font
    integer(c_int32_t), allocatable :: int32Values(:)
    integer(c_int32_t), allocatable :: faceConnections32(:)
    integer(c_int32_t), allocatable :: varTypes(:)
    integer(c_int32_t), allocatable :: passiveVarList(:)
    integer(c_int32_t), allocatable :: valueLocation(:)
    integer(c_int32_t), allocatable :: shareVarFromZone(:)
    integer(c_int32_t), allocatable :: nodeMap32(:)
    integer(c_int32_t), allocatable :: numSegPts(:)
    integer(c_int64_t) :: numValues, numFaceValues
    integer(c_int64_t) :: numFaceConnections
    integer(c_int64_t) :: iMax, jMax, kMax
    integer(c_int64_t), allocatable :: faceConnections64(:)
    integer(c_int64_t), allocatable :: nodeMap64(:)
    real(c_float), allocatable :: floatValues(:)
    real(c_double) :: solutionTime
    real(c_double) :: xtecplot, ytecplot, ztecplot, patternLength, lineThickness
    real(c_double) :: arrowheadAngle, arrowheadSize
    real(c_double) :: geomX, geomY, geomZ, width, height, squareSize
    real(c_double) :: radius, horizontalAxis, verticalAxis
    real(c_double) :: boxLineThickness, boxMargin, angle, lineSpacing
    real(c_double), allocatable :: doubleValues(:)
    real(c_double), allocatable :: xGeomData(:), yGeomData(:), &
                                    zGeomData(:)
    type(c_ptr) :: inputFileHandle = C_NULL_PTR
    type(c_ptr) :: outputFileHandle = C_NULL_PTR
    type(c_ptr) :: stringCPtr = C_NULL_PTR
    type(c_ptr) :: nameCPtr = C_NULL_PTR, valueCPtr = C_NULL_PTR

    character(MAX_STR) :: VarSTR
    character(MAX_STR) :: ZoneSTR
    character(MAX_STR) :: CellCenteredSTR    
    
    ! rgm added for hgs comatibility
    integer :: nln_padded
    integer(c_int32_t), allocatable :: iprop(:)
    integer(c_int32_t), allocatable :: HGS_nodeMap32(:,:)
    integer :: nn, ne, nsptot
    integer :: HGS_nn2D
    integer :: HGS_ne2D
    
    type(c_ptr) :: OutputFileHandle_M(MAXMEDIA) = C_NULL_PTR
    integer(c_int64_t) :: iMax_M(MAXMEDIA), jMax_M(MAXMEDIA), kMax_M(MAXMEDIA)
    integer(c_int32_t) :: varTypes_M(MAXVAR,MAXMEDIA)
    integer(c_int32_t) :: passiveVarList_M(MAXVAR,MAXMEDIA)
    integer(c_int32_t) :: valueLocation_M(MAXVAR,MAXMEDIA)
    integer(c_int32_t) :: shareVarFromZone_M(MAXVAR,MAXMEDIA)
    character(1024) :: labelSet_M(MAXMEDIA)
    
    logical :: yVertical
    
    type TecplotDomain
        ! common to all types of domains: GWF, CLN, SWF, ...
        character(MAX_LBL) :: STR_LengthUnit
        
        logical :: IsDefined=.false.      ! this type of domain has been defined 
        character(128) :: MeshType      ! structured or unstructured?
        character(128) :: ElementType      ! for tecplot, febrick (GWF), fequadrilateral(SWF), felineseg(CLN)

        character(11) :: Name='none'
        integer :: nElements                ! number of Elements in the mesh
        integer :: nLayers                 ! number of layers in the mesh 
        integer :: nNodes               ! number of nodes in the mesh  

        integer :: nNodesPerElement        ! number of nodes/Element  
        integer, allocatable :: iNode(:,:)  ! node list for Element (nElements, nNodesPerElement)
        
        !integer :: nEdgesPerElement        ! number of edges/Element  (for determining neighbours)
        !integer, allocatable :: xEdge(:,:)  ! x-coord of centroid for Element edge(nElements, nNedgesPerElement)
        !integer, allocatable :: yEdge(:,:)  ! y-coord of centroid for Element edge(nElements, nNedgesPerElement)
        !integer, allocatable :: zEdge(:,:)  ! z-coord of centroid for Element edge(nElements, nNedgesPerElement)

        !integer :: iz      ! is 1 if the elevations of node and mesh elements vertices are supplied; 0 otherwise
        !integer :: ic      ! is 1 if the Element specifications associated with each node are supplied; 0 otherwise
        !
           
        !! arrays of size nElements
        real(dr), allocatable :: xElement(:)      ! Element x coordinate
        real(dr), allocatable :: yElement(:)      ! Element y coordinate
        real(dr), allocatable :: zElement(:)      ! Element z coordinate
        !real(dr), allocatable :: Top(:)        ! Element top elevation
        !real(dr), allocatable :: Bottom(:)     ! Element bottom elevation
        integer, allocatable :: iLayer(:)      ! Element layer number
        integer, allocatable :: iZone(:)       ! Element zone number
        
        ! inner circles
        logical :: InnerCircles=.false.      ! Set to true if inner circles are calculated for Tecplot triangles
        real(dr), allocatable :: ElementArea(:)   ! projected area of Element in XY
        real(dr), allocatable :: rCircle(:)    ! projected inner circle radius from GridBldr
        real(dr), allocatable :: xCircle(:)    ! x coordinate of inner circle centre
        real(dr), allocatable :: yCircle(:)   ! y coordinate of inner circle centre
        real(dr), allocatable :: SideLength(:,:) ! projected side length from GridBldr, nNodesPerElement by nElements
        real(dr), allocatable :: xSide(:,:)         ! projected x coordinate of inner circle radius tangent to side
        real(dr), allocatable :: ySide(:,:)         ! projected y coordinate of inner circle radius tangent to side
        
        real(dr), allocatable :: ConnectionLength(:)    ! varialbe CLN in modflow, not to be confused with CLN (Connected Linear Network)
        real(dr), allocatable :: PerpendicularArea(:)   ! FAHL in modflow
        
        real(dr), allocatable :: Length(:) ! length of CLN cell
        real(dr), allocatable :: LowestElevation(:) ! lowest point of CLN cell
        real(dr), allocatable :: SlopeAngle(:) ! angel of CLN cell with horizontal
        
        !
        ! face neighbours
        integer, allocatable :: Element(:)
        integer, allocatable :: face(:)
        integer, allocatable :: neighbour(:)
        
        ! ia ja arrays for Element connections
        integer, allocatable :: njag      ! total number of connections for mesh
        integer, allocatable :: ia(:)      ! size nElements, number of connections/Element
        integer, allocatable :: ja(:)      ! size total number of connections for mesh, Element connection lists
        integer, allocatable :: jaElement(:)      ! size total number of connections for mesh, used for node-centred method only

        ! of size nNodes
        real(dr), allocatable :: x(:) 
        real(dr), allocatable :: y(:)
        real(dr), allocatable :: z(:)
        integer, allocatable :: gwf_nn_from_swf_nn(:)  ! HGS link_olf2pm information
        integer, allocatable :: swf_nn_from_gwf_nn(:)  ! HGS link_pm2olf information 
        
        integer :: nZones                  ! number of zones in domain
        integer,allocatable	:: Element_is(:)  ! size nElements,  bit setting e.g. chosen/not chosen
        integer,allocatable	:: Node_is(:)  ! size nNodes,  bit setting e.g. chosen/not chosen
        integer,allocatable	:: Zone_is(:)  ! size nZones,  bit setting e.g. chosen/not chosen
        
            
    end type TecplotDomain

    

    !*************************************************************old plot_data declarations 
    type HGSTecplotdomain
        character(50) :: Name
        logical :: Exists=.false.
	    integer :: Unit
	    integer	:: CplScheme
        integer :: IDNum

        character(MAX_LBL) :: Plot_str 
	    logical :: Plot=.true.

	    integer :: Nvar
	    character(MAX_STR) :: VarList

	    ! nodes
	    integer :: NN
	    integer, allocatable :: ToPmLink(:)     ! given well node number (1 to NN) return Pm node number (1 to PmNN)
	    integer, ALLOCATABLE :: FromPmLink(:)     ! given pm node number (1 to PmNN) returns  node number (1 to NN)
        integer :: StartIndex    ! position in nunk depending on pre-existing media in domain

	    integer :: Node3DNumNvar

	    real(dr), allocatable :: X(:)    ! x-coordinate
	    integer :: XNvar
	    real(dr), allocatable :: Y(:)    ! y-coordinate
	    integer :: YNvar
	    real(dr), allocatable :: Z(:)    ! z-coordinate
	    integer :: ZNvar
	    

        character(MAX_LBL) :: HeadOutput_str 
	    logical :: HeadOutput=.true.
	    real(dr), allocatable :: Head(:)    
	    real(dr), allocatable :: P(:)    
	    real, allocatable :: Depth(:)    
	    real, allocatable :: LogDepth(:)    
	    integer :: HeadNvar
	    integer :: PNvar
	    logical :: HeadFile
	    logical :: HeadFileN
	    integer :: DepthNvar
	    integer :: LogDepthNvar

        character(MAX_LBL) :: SatOutput_str
	    logical :: SatOutput=.true.
	    real, allocatable :: Sat(:)					
        integer :: SatNvar
	    logical :: SatFile
	    logical :: SatFileN
    	
        character(MAX_LBL) :: IceSatOutput_str
	    logical :: IceSatOutput=.true.
	    real, allocatable :: IceSat(:)					
        integer :: IceSatNvar
	    logical :: IceSatFile
	    logical :: IceSatFileN
    	
        character(MAX_LBL) :: VelOutput_str 
	    logical :: VelOutput=.true.
	    real, allocatable :: Vx(:)                   
        integer :: VxNvar
	    real, allocatable :: Vy(:)                   
        integer :: VyNvar
	    real, allocatable :: Vz(:)                   
        integer :: VzNvar
	    logical :: VelFile
	    logical :: VelFileN

        character(MAX_LBL) :: ConcOutput_str 
	    logical :: ConcOutput=.true.
	    real(dr), allocatable :: Conc(:,:)           
	    integer, allocatable :: ConcNvar(:)   
	    logical :: ConcFile
	    logical :: ConcFileN

        character(MAX_LBL) :: IConcOutput_str 
	    logical :: IConcOutput=.true.
	    real(dr), allocatable :: IConc(:,:)           
	    integer, allocatable :: IConcNvar(:)   
	    logical :: IConcFile
	    logical :: IConcFileN

        character(MAX_LBL) :: PecletOutput_str 
	    logical :: PecletOutput=.true.
	    real, allocatable :: Pecletx(:)                   
        integer :: PecletxNvar
	    real, allocatable :: Peclety(:)                   
        integer :: PecletyNvar
	    real, allocatable :: Pecletz(:)                   
        integer :: PecletzNvar
	    logical :: PecletFile
	    logical :: PecletFileN

 
        character(MAX_LBL) :: DiffPecletOutput_str 
	    logical :: DiffPecletOutput=.true.
	    real, allocatable :: DiffPecletx(:)                   
        integer :: DiffPecletxNvar
	    real, allocatable :: DiffPeclety(:)                   
        integer :: DiffPecletyNvar
	    real, allocatable :: DiffPecletz(:)                   
        integer :: DiffPecletzNvar
	    logical :: DiffPecletFile
	    logical :: DiffPecletFileN

 


        character(MAX_LBL) :: ElemKOutput_str 
	    logical :: ElemKOutput=.true.
	    real(dr), allocatable :: ElemKx(:)                   
        integer :: ElemKxNvar
	    real(dr), allocatable :: ElemKy(:)                   
        integer :: ElemKyNvar
	    real(dr), allocatable :: ElemKz(:)                   
        integer :: ElemKzNvar
	    logical :: ElemKFile
	    logical :: ElemKFileN
        
        ! Time-varying K fields
        character(MAX_LBL) :: TVKOutput_str 
	    logical :: TVKOutput=.true.
	    real, allocatable :: TVKx(:)                   
        integer :: TVKxNvar
	    real, allocatable :: TVKy(:)                   
        integer :: TVKyNvar
	    real, allocatable :: TVKz(:)                   
        integer :: TVKzNvar
	    logical :: TVKFile
	    logical :: TVKFileN


        character(MAX_LBL) :: ElemPorOutput_str 
	    logical :: ElemPorOutput=.true.
	    real, allocatable :: ElemPor(:)					
        integer :: ElemPorNvar
	    logical :: ElemPorFile
	    logical :: ElemPorFileN

        character(MAX_LBL) :: ElemStorOutput_str 
	    logical :: ElemStorOutput=.true.
	    real(dr), allocatable :: ElemStor(:)					
        integer :: ElemStorNvar
	    logical :: ElemStorFile
	    logical :: ElemStorFileN

        character(MAX_LBL) :: ElemTortOutput_str 
	    logical :: ElemTortOutput=.true.
	    real(dr), allocatable :: ElemTort(:)					
        integer :: ElemTortNvar
	    logical :: ElemTortFile
	    logical :: ElemTortFileN

        character(MAX_LBL) :: ElemIbedFractionOutput_str 
	    logical :: ElemIbedFractionOutput=.true.
	    real(dr), allocatable :: ElemIbedFraction(:)					
        integer :: ElemIbedFractionNvar
	    logical :: ElemIbedFractionFile
	    logical :: ElemIbedFractionFileN

        character(MAX_LBL) :: ApertureOutput_str 
	    logical :: ApertureOutput=.true.
	    real(dr), allocatable :: Ap(:)					
        integer, allocatable :: ifrac_3d_elem_map(:,:)  ! temporary because of way apertures are read
        integer :: ApNvar
	    logical :: ApFile
	    logical :: ApFileN

        character(MAX_LBL) :: PermafrostOutput_str 
	    logical :: PermafrostOutput=.true.
	    real, allocatable :: Permafrost(:)					
        integer :: PermafrostNvar
	    logical :: PermafrostFile
	    logical :: PermafrostFileN


        character(MAX_LBL) :: SoilfrostOutput_str 
	    logical :: SoilfrostOutput=.true.

        character(MAX_LBL) :: CompactOutput_str 
	    logical :: CompactOutput=.true.

        character(MAX_LBL) :: DeltaZOutput_str 
	    logical :: DeltaZOutput=.true.
	    integer :: DeltaZNvar
	    logical :: DeltaZFile
	    logical :: DeltaZFileN

        character(MAX_LBL) :: ExchFluxOutput_str 
	    logical :: ExchFluxOutput=.true.
	    real, allocatable :: ExchFlux(:)             
	    integer :: ExchFluxNvar
	    logical :: ExchFluxFile
	    logical :: ExchFluxFileN

        character(MAX_LBL) :: ExchSolOutput_str 
	    logical :: ExchSolOutput=.true.
	    real, allocatable :: ExchSolAdv(:,:)
	    integer, allocatable :: ExchSolAdvNvar(:)
	    logical :: ExchSolAdvFile
	    logical :: ExchSolAdvFileN
	    real, allocatable :: ExchSolDisp(:,:)
	    integer, allocatable :: ExchSolDispNvar(:)
	    logical :: ExchSolDispFile
	    logical :: ExchSolDispFileN

        character(MAX_LBL) :: ETOutput_str
 	    logical :: ETOutput=.true.
	    real(dr), allocatable :: Evap(:)                  ! et surface evaporation
	    integer :: EvapNvar
	    logical :: ETFile
	    logical :: ETFilen
	    real(dr), allocatable :: PmEvap(:)                 
	    integer :: PmEvapNvar
	    real(dr), allocatable :: PmTranspire(:)            
	    integer :: PmTranspireNvar
	    real(dr), allocatable :: ETTotal(:)                
	    integer :: ETTotalNvar

        character(MAX_LBL) :: Depth2GWTOutput_str 
	    logical :: Depth2GWTOutput=.true.
	    real, allocatable :: Depth2GWT(:)					
        integer :: Depth2GWTNvar
	    logical :: Depth2GWTFile
	    logical :: Depth2GWTFileN

        integer, allocatable :: Fslice(:)					
        integer :: FsliceNvar

        
        character(MAX_LBL) :: DomainTruncate_str 
	    logical :: DomainTruncate=.false.
        real*8	:: XminTrunc=-1.e20,XmaxTrunc=1.e20
        real*8	:: YminTrunc=-1.e20,YmaxTrunc=1.e20
        real*8	:: ZminTrunc=-1.e20,ZmaxTrunc=1.e20
        integer :: NN_u, NE_u
 	    logical, allocatable :: UseN(:)                     ! use node
	    logical, allocatable :: UseE(:)                     ! use element
	    integer, allocatable :: TruncIndx(:)               ! truncated grid has new node numbers

	    ! elements 
	    integer :: NE
	    integer :: NLN
	    integer, allocatable :: IN(:,:)   
	    integer, allocatable :: ZoneNum(:)  
        integer :: ZoneNvar
        character(15) :: ZoneType     ! string for tecplot e.g. FEBRICK

	end type HGSTecplotdomain


    ! General parameters 

	logical :: TitleFileExists
    logical :: PlotControlFileExists
    integer :: PlotControlUnit
    integer :: GlobalStartIndex=0
    logical :: OutputExists

    character(MAX_INST) :: plot_instruction

	! I/O file unit numbers
	integer :: itmp2 = 0    ! temporary file unit number
	integer :: ieco = 0
	integer :: idbg = 0
	character(MAX_STR) :: filename

	character(MAX_LBL), allocatable :: title(:)
	integer :: ntit
	character(MAX_LBL) :: message
	character(MAX_LBL) :: label,string

	character(MAX_LBL) :: timestamp
    real(dr) :: TecplotTimeOffset=0.0d0
	
	!character(512) :: line,dlist,temp_line  ! rgm2020 
	character(MAX_STR) :: dlist,temp_line
	
    character(MAX_STR) :: zone_line
    
	character(4) :: nfs     ! string to hold output time extension e.g. 0001
    character(6) :: NFSFormat='(i4.4)'      ! format for writing integer to time extension string


    !character(40) :: cfprfx,prefix ! rgm2020
	character(MAX_LBL) :: cfprfx,LocalPrefix
	integer :: len_prfx

    integer :: nln_f


	integer :: nln
	logical :: blockel
	logical :: tetra	!DB dec-06 
	integer :: len
	integer :: nfile



    real*8	:: sumx, sumy, sumz
    real*8	:: xc, yc, zc


	logical :: ex


    
	! compare heads
    real*8	:: xmin_trunc_h,xmax_trunc_h
    real*8	:: ymin_trunc_h,ymax_trunc_h
    real*8	:: zmin_trunc_h,zmax_trunc_h



	
	!rt 2006-06.07
    real(dr) :: zero_dr=0.0
    real(dr) :: small_dr=1.e-10


    integer :: nx, ny, nz

	! time domain - porous medium
    real*8	:: tmin_trunc,tmax_trunc

	logical :: SpeciesFileExists=.false.
    integer :: nspeciesmob
    character(50), allocatable  :: spname(:)
    integer :: lname

	logical :: ObservedHeadsFileExists
    integer :: nobs_head
	real(dr), allocatable :: xob(:)                     ! x-coordinate
	real(dr), allocatable :: yob(:)                     ! y-coordinate
	real(dr), allocatable :: zob(:)                     ! z-coordinate
	real(dr), allocatable :: hob(:)                     ! z-coordinate

    REAL(dr), allocatable :: BasisFunctions(:,:)        ! basis functions for obs point interpolation
    integer, allocatable :: obs_el(:)                  ! element containing obs point




    character(MAX_INST), parameter	:: tecplot_mode_str = 'tecplot mode'
	logical :: tecplot_mode=.true.

    character(MAX_INST), parameter	:: gms_mode_str = 'gms mode'
	logical :: gms_mode=.false.

    character(MAX_INST), parameter	:: double_xy_str = 'double xy'
	logical :: double_xy=.false.

   character(MAX_INST), parameter	:: truncate_3d_domain_str = 'truncate 3d domain'
	logical :: truncate_3d_domain=.false.

    character(MAX_INST), parameter	:: isolate_node_str = 'isolate node'
	logical :: isolate_node=.false.

    character(MAX_INST), parameter	:: truncate_time_domain_str = 'truncate time domain'
	logical :: truncate_time_domain=.false.
    
    character(MAX_INST), parameter	:: compare_heads_3d_domain_str = 'compare heads 3d domain'
	logical :: compare_heads_3d_domain=.false.




	!DB apr-07 
	integer :: nvar_vxtet
	integer :: nvar_vytet
	integer :: nvar_vztet

	real, allocatable :: vxtet(:)            
	real, allocatable :: vytet(:)            
	real, allocatable :: vztet(:)            

 
!    character(MAX_INST), parameter	:: truncate_time_domain_str = 'truncate time domain'
!	logical :: truncate_time_domain=.false.

	integer :: iso_node, n_extend
    
    ! Tecplot binary variable data stored here before we know medium.NVar
    integer :: TBinary_vartype(MAXVAR)
    integer :: TBinary_passiveVarList(MAXVAR)
    integer :: TBinary_valueLocation(MAXVAR)
    integer :: TBinary_shareVarFromZone(MAXVAR)
    
    integer :: iMedium=0
    
    logical :: Wells
    logical :: FindSecondOccurrence

	real, parameter	  :: DepthMin = 1.0d-8
!*************************************************************************old plot_data declarations  end here
    
    
    
    contains
    
   !------------------------------------------------------------------------
   subroutine BuildTecplot(FnumMUT) !--- Process Tecplot instructions
        implicit none
    
        integer :: FnumMUT
    
        do
            read(FnumMUT,'(a)',iostat=status,end=10) Tecplot_CMD
            call Msg('!-----------------------')
            call Msg('Tecplot:'//Tecplot_CMD)
            
            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.Tecplot'
		        write(ErrStr,'(a)') trim(ErrStr)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
            end if
            
            if(index(Tecplot_CMD, Tecplot_hsplot2012_CMD) /= 0) then
                call Tecplot_hsplot2012
            else if(index(Tecplot_CMD, TecplotTimeOffset_CMD) /= 0) then
                read(FnumMUT,*) TecplotTimeOffset

            else if(index(Tecplot_CMD, Tecplot_ReadWrite_szplt_CMD) /= 0) then
                call Tecplot_ReadWrite_szplt(FnumMUT)

            else if(index(Tecplot_CMD, Tecplot_ASCIIMode_CMD) /= 0) then
                BINARYMode=.false.
            
            else if(index(Tecplot_CMD, Tecplot_BinaryPLTMode_CMD) /= 0) then
                FileFormat=0
                
            else if(index(Tecplot_CMD, Tecplot_End_CMD)  /= 0) then
                exit
            
            else
                call ErrMsg('Tecplot?:'//Tecplot_CMD)
            end if
        end do

        10 continue        
   
    end subroutine BuildTecplot
    !*************************************************************************old plot_data routines 
    !------------------------------------------------------------------------
    subroutine Tecplot_hsplot2012
	    !
	    ! Purpose:
	    !   Obtain a Hydrosphere data set prefix, read all files associated with
	    !   prefix containing nodal head, saturation and concentration data
	    !   (i.e. .head.0001, .saturation.0001 and .cconcentration.0001 etc) and then write tecplot compatible
	    !   file called prefixo.dat which also includes material numbers.

     
	    use version
         
	    implicit none
        integer :: i, j

        real(dr) :: t_test
        ! structures to hold data for each domain type 
        type(HGSTecplotdomain) Global  
        type(HGSTecplotdomain) MyPm  
        type(HGSTecplotdomain) MyDual
        type(HGSTecplotdomain) MyFrac
        type(HGSTecplotdomain) MyOlf 
        type(HGSTecplotdomain) MyWell 
        type(HGSTecplotdomain) MyChan 
        type(HGSTecplotdomain) MyTile 
        
        

        call getunit(itmp)
        call enter_prefix(LocalPrefix,len_prfx,itmp,'.grok')
        call freeunit(itmp)
    

        call TitleRead
	    call SpeciesRead

        call DomainCreate(MyPm,'pm',MyPm)   ! must call porous media first as coords are used for other media
        ! Use multi coupling scheme so files are read e.g. call HeadInit.
        MyPm.CplScheme=multi
        ! Pm considered to be global medium so copy.
        Global=MyPm
        call DomainCreate(MyDual,'dual',Global)
        call DomainCreate(MyFrac,'frac',Global)
        call DomainCreate(MyOlf,'olf',Global)
        call DomainCreate(MyWell,'well',Global)
        call DomainCreate(MyChan,'chan',Global)
        call DomainCreate(MyTile,'tile',Global)

 
        call getunit(itmp)
        open(itmp,file='scratch_plot',status='replace')
        call getunit(itmp2)

	    !call getunit(ieco,'o.hsplot.eco')
	    !open(ieco,file=trim(LocalPrefix)//'o.hsplot.eco',status='unknown')
        ieco=FNumEco

        inquire(file=trim(LocalPrefix)//'.plot.control',exist=PlotControlFileExists)
        call getunit(PlotControlUnit)
        if(PlotControlFileExists) then
            open(itmp2,file=trim(LocalPrefix)//'.plot.control',status='unknown')
            call strip_comments(itmp2,itmp)
            close(itmp)
            call freeunit(itmp2)
            open(PlotControlUnit,file='scratch_plot',status='unknown')
            call ReadPlotControlInit   
            call ReadPlotControl(MyPm)   
            call ReadPlotControl(MyDual)
            call ReadPlotControl(MyFrac)
            call ReadPlotControl(MyOlf)
            call ReadPlotControl(MyWell)
            call ReadPlotControl(MyChan)
            call ReadPlotControl(MyTile)
        else
            open(PlotControlUnit,file=trim(LocalPrefix)//'.plot.control',status='unknown')
            call WritePlotControlInit   
            call WritePlotControl(MyPm)   
            call WritePlotControl(MyDual)
            call WritePlotControl(MyFrac)
            call WritePlotControl(MyOlf)
            call WritePlotControl(MyWell)
            call WritePlotControl(MyChan)
            call WritePlotControl(MyTile)
        end if
        call freeunit(PlotControlUnit)


	    ! Time loop
	    nfile=1
	    do
		    write(nfs,'(i4.4)') nfile
	        call GetTimeStamp('.'//nfs,global,OutputExists)
        
  
            if(OutputExists) then 
		        if(truncate_time_domain) then
			        read(timestamp,*) t_test
			        if(t_test < tmin_trunc .or. t_test > tmax_trunc) then
		                call Msg( 'SKIP Time '//trim(nfs)//': '//trim(TimeStamp))
				        cycle
			        else
		                call Msg(' ' )
		                call Msg('----------------------------------------- ' )
		                call Msg('Time '//trim(nfs)//': '//trim(TimeStamp))
			        end if
		        else
		            call Msg(' ' )
                    call Msg('--------------------------------------------- ' )
	                call Msg('Time '//trim(nfs)//': '//trim(TimeStamp))
		        end if
		    end if

        
            if(nfile==1) then
                call Init(MyPm,Global)
                ! Update e.g. FileN's may have changed 
                Global=MyPm

                call Init(MyDual,Global)
                call Init(MyFrac,Global)
                call Init(MyOlf,Global)
                call Init(MyWell,Global)
                call Init(MyChan,Global)
                call Init(MyTile,Global)
                
                                
                if(.not. OutputExists) then 
                    call Msg(' No output for Time '//trim(nfs))
                    if(BinaryMode) then
                        do j=1,iMedium
                            i = tecFileWriterClose(OutputFileHandle_M(j))
                        end do
                    end if
                    exit
                end if
            else
                if(.not. OutputExists) then
                    call Msg(' No output for Time '//trim(nfs))
                    if(BinaryMode) then
                        do j=1,iMedium
                            i = tecFileWriterClose(OutputFileHandle_M(j))
                        end do
                    end if
                    exit
                end if
                call Append(MyPm,global)
                ! Update e.g. FileN's may have changed 
                Global=MyPm

                call Append(MyDual,global)
                call Append(MyFrac,global)
                call Append(MyOlf,global)
                call Append(MyWell,global)
                call Append(MyChan,global)
                call Append(MyTile,global)
                
            end if 

            nfile=nfile+1

        end do
        
        

	    !! simulated vs observed head comparison
	    !if(compare_heads_3d_domain) then
		   ! call read_obs_heads(global)
     !       call compare_heads_output(global)
     !   end if

	    call Msg(' ')
	    call Msg('---- Normal exit ----')
        !call freeunit(ieco)
    

    end subroutine Tecplot_hsplot2012
    !------------------------------------------------------------------------
    subroutine Init(medium,global)
         
        implicit none
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global

        if(medium.Exists .and. medium.Plot) then 
            iMedium=iMedium+1
            Medium.IDNum=iMedium

            call Msg('------------------------------------------------')

      
            if(medium.CplScheme == multi) then
                call MultiTruncate(medium,global)
                call InitMultiTecplot(medium,global)
            else
                call CommonTruncate(medium,global)
                call InitCommonTecplot(medium,global)
            end if
            
        end if
    end subroutine Init
    !------------------------------------------------------------------------
    subroutine Append(medium,global)
         
        implicit none
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global
    
        if(medium.Exists .and. medium.Plot) then 
            if(medium.CplScheme == multi) then
                call AppendMultiTecplot(medium)
            else
                call AppendCommonTecplot(medium,global)
            end if
        end if
    end subroutine Append
    !------------------------------------------------------------------------
    subroutine DomainCreate(NewDomain,NewDomainLable,global)
         
        implicit none
        type(HGSTecplotdomain) NewDomain
        type(HGSTecplotdomain) global
        character*(*) :: NewDomainLable 
    
        filename=trim(LocalPrefix)//'o.coordinates_'//NewDomainLable
        inquire(file=filename , exist=NewDomain.Exists)

        if(.not. NewDomain.Exists) return
    
        NewDomain.Name = NewDomainLable
    
        NewDomain.Plot_str = 'write '//NewDomainLable//' domain file'
        NewDomain.Plot=.true.
    
        ! all possible outputs are included even though a specific domain may not use it
        ! e.g. depth for fracture domain 
        NewDomain.HeadOutput_str = 'no '//NewDomainLable//' heads'
        NewDomain.HeadOutput=.true.

        NewDomain.SatOutput_str = 'no '//NewDomainLable//' saturations'
        NewDomain.SatOutput=.true.

        NewDomain.IceSatOutput_str = 'no '//NewDomainLable//' ice saturation'
        NewDomain.IceSatOutput = .true.
    
        NewDomain.VelOutput_str = 'no '//NewDomainLable//' linear velocities (vx, vy, vz)'
        NewDomain.VelOutput=.true.

        NewDomain.ConcOutput_str = 'no '//NewDomainLable//' concentrations'
        NewDomain.ConcOutput=.true.

        NewDomain.IConcOutput_str = 'no '//NewDomainLable//' immobile/isotope fractionation concentrations'
        NewDomain.IConcOutput=.true.

        NewDomain.PecletOutput_str = 'no '//NewDomainLable//' peclet number'
        NewDomain.PecletOutput=.true.

        NewDomain.DiffPecletOutput_str = 'no '//NewDomainLable//' diffusion peclet number'
        NewDomain.DiffPecletOutput=.true.

        NewDomain.ElemKOutput_str = 'no '//NewDomainLable//' element k'
        NewDomain.ElemKOutput=.true.
    
        NewDomain.TVKOutput_str = 'no '//NewDomainLable//' time-varying element k'
        NewDomain.TVKOutput=.true.
    
        NewDomain.ElemPorOutput_str = 'no '//NewDomainLable//' element porosity'
        NewDomain.ElemPorOutput=.true.

        NewDomain.ElemStorOutput_str = 'no '//NewDomainLable//' element storativity'
        NewDomain.ElemStorOutput=.true.

        NewDomain.ElemTortOutput_str = 'no '//NewDomainLable//' element tortuosity'
        NewDomain.ElemTortOutput=.true.

        NewDomain.ElemIbedFractionOutput_str = 'no '//NewDomainLable//' element fraction of compressible interbeds'
        NewDomain.ElemIbedFractionOutput=.true.

        NewDomain.ApertureOutput_str = 'no '//NewDomainLable//' aperture'
        NewDomain.ApertureOutput=.true.

        NewDomain.PermafrostOutput_str = 'no '//NewDomainLable//' permafrost'
        NewDomain.PermafrostOutput=.true.
    
        NewDomain.SoilfrostOutput_str = 'no '//NewDomainLable//' soil frost'
        NewDomain.SoilfrostOutput=.true.
    
        NewDomain.CompactOutput_str = 'no '//NewDomainLable//' compaction'
        NewDomain.CompactOutput=.true.

        NewDomain.DeltaZOutput_str = 'no '//NewDomainLable//' delta z'
        NewDomain.DeltaZOutput=.true.

        NewDomain.ExchFluxOutput_str = 'no '//NewDomainLable//'/pm exchange flux'
        NewDomain.ExchFluxOutput=.true.
 
        NewDomain.ExchSolOutput_str = 'no '//NewDomainLable//'/pm solute exchange'
        NewDomain.ExchSolOutput=.true.

        NewDomain.ETOutput_str  = 'no '//NewDomainLable//' et'
        NewDomain.ETOutput=.true.

        NewDomain.Depth2GWTOutput_str  = 'no '//NewDomainLable//' depth to water table'
        NewDomain.Depth2GWTOutput=.true.
    
        NewDomain.DomainTruncate_str = 'truncate '//NewDomainLable//' domain'
        NewDomain.DomainTruncate=.false.

        call Msg(' ')
        call Msg('----------------------------------- ')
        call Msg('Creating domain: '//NewDomainLable)
        call CoordsRead(NewDomain,global)
        call ElemsRead(NewDomain)
    
        if(SpeciesFileExists) then ! .and. NewDomain.CplScheme == shared) then ! must allocate ConcNvar here 
	        allocate(NewDomain.ConcNvar(nspeciesmob),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate NewDomain.ConcNvar')
		        stop
	        end if
	        NewDomain.ConcNvar(:) = 0 
        end if
    end subroutine DomainCreate

    !------------------------------------------------------------------------
    subroutine CoordsRead(medium,global)
         
        implicit none
        integer :: i
 
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global

        filename=trim(LocalPrefix)//'o.coordinates_'//medium.name
        call Msg('Coordinates file: '//filename)

        call getunit(itmp)
        OPEN(itmp,file=filename, &
	        status = 'old', &
	        action = 'read', &
	        form = 'unformatted', &
	        iostat = status)
        if(status /= 0) then
	        call Msg('FILE ERROR: '//filename)
	        stop
        end if

        read(itmp) medium.NN
    
        if(medium.name == 'pm') then
            write(TMPStr,*) medium.NN
            call Msg('Number of nodes: '//trim(TMPStr))
            allocate(medium.X(medium.NN),medium.Y(medium.NN),medium.Z(medium.NN),stat=status)
            if(status /= 0) then
	            call Msg('ALLOCATION ERROR: Arrays x,y,z')
	            stop
            end if
            medium.X = 0 ! automatic initialization
            medium.Y = 0 ! automatic initialization
            medium.Z = 0 ! automatic initialization
            read(itmp) (medium.X(i),medium.Y(i),medium.Z(i),i=1,medium.NN)
            read(itmp) nx,ny,nz
            call freeunit(itmp)
        
            medium.StartIndex=0
            GlobalStartIndex=medium.NN
        else
            read(itmp) medium.CplScheme
            allocate(medium.ToPmLink(medium.NN),medium.FromPmLink(global.NN),stat=status)
            if(status /= 0) then
	            call Msg('Failed to allocate well link arrays')
	            stop
            end if
            medium.ToPmLink(:) = 0 
            medium.FromPmLink(:) = 0 
            read(itmp) (medium.ToPmLink(i),i=1,medium.NN)
            read(itmp) (medium.FromPmLink(i),i=1,global.NN)
            call freeunit(itmp)
        
            if(medium.CplScheme == multi) then
                call Msg('Using multi-node coupling scheme')
            else
                call Msg('Using common-node coupling scheme')
            end if
            
            allocate(medium.X(medium.NN),medium.Y(medium.NN),medium.Z(medium.NN),stat=status)
            if(status /= 0) then
	            call Msg('Failed to allocate well coord arrays')
	            stop
            end if
            medium.X(:) = 0 
            medium.Y(:) = 0 
            medium.Z(:) = 0 

            ! Well node coordinates for multi coupling scheme
            do i=1,medium.NN
	            medium.X(i)=global.X(medium.ToPmLink(i))
	            medium.Y(i)=global.Y(medium.ToPmLink(i))
	            medium.Z(i)=global.Z(medium.ToPmLink(i))
            end do

            medium.StartIndex=GlobalStartIndex
            if(medium.CplScheme == multi) GlobalStartIndex=GlobalStartIndex+medium.NN
        end if
        
    end subroutine CoordsRead

    !------------------------------------------------------------------------
    subroutine ElemsRead(medium)
         
        implicit none
        integer :: i, j
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.elements_'//medium.name
        call Msg('Elements file: '//filename)

        call getunit(itmp)
            OPEN(itmp,FILE = filename, &
	        action = 'read', &
	        form = 'unformatted', &
	        iostat = status)
        if(status /= 0) then
	        call Msg('FILE ERROR: '//filename)
	        stop
        end if

        read(itmp) medium.NLN
        write(TMPStr,*) medium.NLN
        call Msg('Number of nodes per element: '//trim(TMPStr))
        
        read(itmp) medium.NE
        write(TMPStr,*) medium.NE
        call Msg('Number of elements: '//trim(TMPStr))

        allocate(medium.IN(medium.NLN,medium.NE),medium.ZoneNum(medium.NE),stat=status)
        if(status /= 0) then
	        call Msg('Failed to allocate '//trim(medium.name)//' element arrays')
	        stop
        end if
        medium.IN(:,:)=0
        medium.ZoneNum(:) = 0 ! automatic initialization
        read(itmp) ((medium.IN(j,i),j=1, medium.NLN),i=1,medium.NE)
        read(itmp) (medium.ZoneNum(i),i=1,medium.NE)
    
        if(medium.name == 'frac') then
            !fracture element files contain the apertures, which for some reason are not treated like any other 
            !potentially zoned material property.  For now we will just read them here, along with the uneeded
            !ifrac_3d_elem_map
            allocate(medium.ifrac_3d_elem_map(medium.NE,2),medium.ap(medium.NE),stat=status)
            if(status /= 0) then
                call Msg('Failed to allocate '//trim(medium.name)//' element arrays')
                stop
            end if
            medium.ifrac_3d_elem_map(:,:)=0
            medium.ap(:) = 0 
            read(itmp) ((medium.ifrac_3d_elem_map(i,j),i=1,medium.NE),j=1,2)
            read(itmp) (medium.ap(i),i=1,medium.NE)
        end if

    
        call freeunit(itmp)
        
    
        select case (medium.NLN)
        case (2)
            medium.ZoneType='FELINESEG'
        case (3,4)
            medium.ZoneType='FEQUADRILATERAL'  ! i.e. 3-node 2D elements written as quads with 1 repeated node
            if(medium.name == 'pm' .or. medium.name == 'dual') medium.ZoneType='FETETRAHEDRON'   ! i.e. 4-node 3D elements are tetrahedra
        case (6,8)
            medium.ZoneType='FEBRICK'
        end select
        call Msg('Zonetype: '//medium.ZoneType)

        
    end subroutine ElemsRead
    !------------------------------------------------------------------------
    subroutine CommonTruncate(medium,global)
         
        implicit none
        integer :: i, j
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global

        allocate(medium.UseN(global.NN),medium.UseE(medium.NE),medium.TruncIndx(global.NN),stat=status)
        if(status /= 0) then
	        call Msg('ALLOCATION ERROR: medium.UseN, medium.UseE, medium.TruncIndx')
	        stop
        end if
        medium.UseN(:) = .false.  ! flag which 3D nodes are used if truncating
        medium.UseE(:) = .false.  ! flag which tile elements are used if truncating
        medium.TruncIndx(:) = 0  ! list of new node #'s

        if(medium.DomainTruncate) then
	        do i=1,medium.NE ! compute element centroid
		        sumx=0.0
		        sumy=0.0
		        sumz=0.0
		        nln=0
		        do j=1,medium.NLN
			        if(medium.IN(j,i) > 0) then
				        nln=nln+1
				        sumx=sumx+global.X(medium.IN(j,i))
				        sumy=sumy+global.Y(medium.IN(j,i))
				        sumz=sumz+global.Z(medium.IN(j,i))
			        end if
		        end do
		        xc=sumx/real(nln)
		        yc=sumy/real(nln)
		        zc=sumz/real(nln)
		        if(	xc < medium.XminTrunc .or. xc > medium.XmaxTrunc .or. &    ! don't use element if outside truncate limits
			        yc < medium.YminTrunc .or. yc > medium.YmaxTrunc .or. &
			        zc < medium.ZminTrunc .or. zc > medium.ZmaxTrunc         ) then

			        medium.UseE(i)=.false.
		        else ! flag nodes
			        medium.UseE(i)=.true.
			        do j=1,medium.NLN
				        medium.UseN(medium.IN(j,i))=.true.   ! i.e. medium.UseN is dimensioned to medium.NN
			        end do
		        end if
	        end do

            medium.NE_u=0   ! count elements used
	        do i=1,medium.NE
		        if(medium.UseE(i)) medium.NE_u=medium.NE_u+1
	        end do

            medium.NN_u=0   ! count nodes used
	        do i=1,global.NN
		        if(medium.UseN(i)) then
			        medium.NN_u=medium.NN_u+1
			        ! old tile node i is now node medium.NN_u
			        medium.TruncIndx(i)=medium.NN_u
		        end if
	        end do
            call Msg(' ')
            call Msg('TRUNCATED MESH:')
            write(TMPStr,*) medium.XminTrunc,medium.XmaxTrunc
	        call Msg(TMPStr)
            write(TMPStr,*) medium.YminTrunc,medium.YmaxTrunc
	        call Msg(TMPStr)
            write(TMPStr,*) medium.ZminTrunc,medium.ZmaxTrunc
	        call Msg(TMPStr)
            write(TMPStr,'(i10,a,i10)') medium.NN_u,'/',medium.NN
	        call Msg( ' Nodes used    '//trim(TMPStr))
            write(TMPStr,'(i10,a,i10)') medium.NE_u,'/',medium.NE
	        call Msg( ' Elements used '//trim(TMPStr))
        else
            medium.NN_u=medium.NN
            medium.NE_u=medium.NE
            medium.UseE(:)=.true.

            if(medium.Name == 'pm')  then
                medium.UseN(:)=.true.
	            do i=1,medium.NN
		            medium.TruncIndx(i)=i
	            end do
            else
                medium.UseN(:)=.false.
	            do i=1,medium.NN
		            medium.TruncIndx(medium.ToPmLink(i))=i
	            end do
	            do i=1,medium.NE
		            do j=1,medium.NLN
			            if(medium.IN(j,i) /= 0) medium.UseN(medium.IN(j,i))=.true.   ! i.e. medium.UseN is dimensioned to PmNN
		            end do
		        end do
    
                medium.DomainTruncate=.true.
	
		    end if
        end if


    end subroutine CommonTruncate
    !------------------------------------------------------------------------
    subroutine InitCommonTecplot(medium,global)
         
        implicit none
        integer :: i, j, k, l
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global

        if(BinaryMode) then 
            if(Fileformat==1) then
                OutputFileName=trim(LocalPrefix)//'o.'//trim(medium.name)//'.szplt'// C_NULL_CHAR
            else if (Fileformat==0) then
                OutputFileName=trim(LocalPrefix)//'o.'//trim(medium.name)//'.plt'// C_NULL_CHAR
            end if
            
        else
            Outputfilename=trim(LocalPrefix)//'o.'//trim(medium.name)//'.dat'
            call OpenAscii(medium.Unit,Outputfilename)
            write(medium.Unit,'(a)') 'TITLE = "'//title//'"'
            ! First (i.e. fixed) part of variable list 
            medium.VarList='VARIABLES = '
       end if
    
        call Msg(' ')
        call Msg(trim(medium.name)//' data to tecplot')
        call Msg('Creating tecplot file '//trim(Outputfilename))

        medium.Nvar=0
        call TecplotVarAdd('X',medium.Nvar,medium.XNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Y',medium.Nvar,medium.YNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Z',medium.Nvar,medium.ZNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Zone '//trim(medium.name),medium.Nvar,medium.ZoneNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=3 ! integer 
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=0 ! cell-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        if(medium.Name == 'pm') call FsliceInit(medium)

        if(BinaryMode) then
            continue

        else    
            ! First part of zone line
            if(double_xy) then
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)//', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', DT=(DOUBLE DOUBLE), VARLOCATION=([',medium.ZoneNvar 
            else
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)//', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', VARLOCATION=([',medium.ZoneNvar 
            end if
        end if

        ! Second (i.e. variable dependent) part of variable list
        if(medium.HeadOutput .and. global.HeadFile) then
            call TecplotVarAdd('Head',medium.Nvar,medium.HeadNvar,medium.VarList)
            TBinary_vartype(medium.Nvar)=2 ! double precision
            TBinary_passiveVarList(medium.Nvar)=0
            TBinary_valueLocation(medium.Nvar)=1 ! node-centred
            TBinary_shareVarFromZone(medium.Nvar)=0
        end if
    
        if(medium.SatOutput .and. global.SatFile) then
            call TecplotVarAdd('Saturation',medium.Nvar,medium.SatNvar,medium.VarList) 
            TBinary_vartype(medium.nvar)=1  !single precision
            TBinary_passiveVarList(medium.nvar)=0
            TBinary_valueLocation(medium.nvar)=1 ! node-centred
            TBinary_shareVarFromZone(medium.nvar)=0
        else
            medium.SatOutput=.false.  ! no saturation file
        end if
    
        if(medium.IceSatOutput .and. global.IceSatFile) then
            call TecplotVarAdd('Ice Saturation',medium.Nvar,medium.IceSatNvar,medium.VarList)
            TBinary_vartype(medium.nvar)=1  !single precision
            TBinary_passiveVarList(medium.nvar)=0
            TBinary_valueLocation(medium.nvar)=1 ! node-centred
            TBinary_shareVarFromZone(medium.nvar)=0
        else
            medium.IceSatOutput=.false.
        end if
    
        if(medium.VelOutput) call VelInit(medium)

        if(SpeciesFileExists .and. medium.ConcOutput .and. global.ConcFile) then
            do i=1,nspeciesmob
                call TecplotVarAdd(trim(spname(i)),medium.Nvar,medium.ConcNvar(i),medium.VarList)
                TBinary_vartype(medium.Nvar)=2 ! double precision
                TBinary_passiveVarList(medium.Nvar)=0
                TBinary_valueLocation(medium.Nvar)=1 ! node-centred
                TBinary_shareVarFromZone(medium.Nvar)=0
            end do
        end if

        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
            call TecplotVarAdd('Aperture',medium.Nvar,medium.ApNvar,medium.VarList)
            TBinary_vartype(medium.Nvar)=2 ! double precision
            TBinary_passiveVarList(medium.Nvar)=0
            TBinary_valueLocation(medium.Nvar)=0 ! cell-centred
            TBinary_shareVarFromZone(medium.Nvar)=0
            
            if(.not.BinaryMode) then
                write(temp_line,'(a,i7)') ',',medium.ApNvar
                zone_line=trim(zone_line)//temp_line
            end if
        end if

        call TecplotVarAdd('3DNode#',medium.Nvar,medium.Node3DNumNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=1 ! single precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        if(BinaryMode) then
             dataSetTitle=trim(title(1))//C_NULL_CHAR
            varnames=trim(medium.VarList)//C_NULL_CHAR
            
            read(timestamp,*) SolutionTime
            StrandID=1
            
            
            fileType=0
            
            i = tecFileWriterOpen(OutputFileName, dataSetTitle, varNames, &
                fileFormat, fileType, 1, C_NULL_PTR, OutputFileHandle_M(medium.IDNum))
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecFileWriterOpen returned '//trim(TMPStr))
            else
                call Msg('tecFileWriterOpen success')
            end if
            i = tecFileSetDiagnosticsLevel(OutputFileHandle_M(medium.IDNum), outputDebugInfo)
             if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecFileSetDiagnosticsLevel returned '//trim(TMPStr))
            else
                call Msg('tecFileSetDiagnosticsLevel success')
            end if
            
            zoneTitle=trim(medium.name)//C_NULL_CHAR
            select case (medium.nln)
            case (2)  ! line
                nln_padded=2
                Zonetype=1
            case (4)  ! tetrahedra or 4-node quadrilateral
                nln_padded=4
                if(medium.name == 'pm' .or. medium.name == 'dual') then   ! 3D element so FETETRAHEDRON
                    Zonetype=4
                else  ! 2D element so FEQUADRILATERAL
                    Zonetype=3
                end if
            case (6)  ! prisms
                nln_padded=8
                Zonetype=5
            case (8)  ! blocks
                nln_padded=8
                Zonetype=5
            case default
                write(TMPStr,*) medium.nln
                call ErrMsg('nln not recognized: '//trim(TMPStr))
            end select    
            imax_M(medium.IDNum)=medium.nn_u
            jmax_M(medium.IDNum)=medium.ne_u
            
            do i=1,medium.Nvar
                VarTypes_M(i,medium.IDNum)=TBinary_VarType(i)
                passiveVarList_M(i,medium.IDNum)=TBinary_passiveVarList(i)
                valueLocation_M(i,medium.IDNum)=TBinary_valueLocation(i)
                shareVarFromZone_M(i,medium.IDNum)=TBinary_shareVarFromZone(i)
            end do
            
            numFaceConnections=0
            faceNeighborMode=0
            shareConnectivityFromZone=0
            
            i = tecZoneCreateFE(OutputFileHandle_M(medium.IDNum), zoneTitle, zoneType, &
                imax_M(medium.IDNum), jMax_M(medium.IDNum), varTypes_M(:,medium.IDNum), shareVarFromZone_M(:,medium.IDNum), &
                valueLocation_M(:,medium.IDNum), passiveVarList_M(:,medium.IDNum), &
                shareConnectivityFromZone, numFaceConnections, &
                faceNeighborMode, outputZone)
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecZoneCreateFE returned '//trim(TMPStr))
            else
                call Msg('tecZoneCreateFE success')
            end if
            
            
            i = tecZoneSetUnsteadyOptions(OutputFileHandle_M(medium.IDNum), &
                outputZone, solutionTime, strandID)
            
            
            call BuildCustomZoneNames(medium,global)
            i = tecCustomLabelsAddSet(outputFileHandle_M(medium.IDNum), labelSet_M(medium.IDNum))
            
        else
            write(medium.Unit,'(a)') medium.VarList
            zone_line=trim(zone_line)//']=CELLCENTERED)'
            write(medium.Unit,'(a)') zone_line
        end if
    
        ! Output data to tecplot file
        !call write_tecplot_darray(global.X,global.NN,medium.UseN,'# x',medium.DomainTruncate,medium.Unit)
        call write_tecplot_darray2(global.X,medium.XNvar,global.NN,medium.UseN,'# x',medium.DomainTruncate,medium.Unit,medium.IDNum)
        !call write_tecplot_darray(global.Y,global.NN,medium.UseN,'# y',medium.DomainTruncate,medium.Unit)
        call write_tecplot_darray2(global.Y,medium.YNvar,global.NN,medium.UseN,'# y',medium.DomainTruncate,medium.Unit,medium.IDNum)
        !call write_tecplot_darray(global.Z,global.NN,medium.UseN,'# z',medium.DomainTruncate,medium.Unit)
        call write_tecplot_darray2(global.Z,medium.ZNvar,global.NN,medium.UseN,'# z',medium.DomainTruncate,medium.Unit,medium.IDNum)
        !call write_tecplot_iarray(medium.ZoneNum,medium.NE,medium.UseE,'# zone (cell centred)',medium.DomainTruncate,medium.Unit)
        call write_tecplot_iarray2(medium.ZoneNum,medium.ZoneNvar,medium.NE,medium.UseE,'# zone (cell centred',medium.DomainTruncate,medium.Unit,medium.IDNum)
        if(medium.Name == 'pm') call write_tecplot_iarray2(medium.Fslice,medium.FsliceNvar,medium.NN,medium.UseN,'# feflow slice',medium.DomainTruncate,medium.Unit,medium.IDNum)

        if(medium.HeadOutput .and. global.HeadFile) then
            !call write_tecplot_darray(global.Head,global.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit)
            call write_tecplot_darray2(global.Head,medium.HeadNvar,global.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if


        if(medium.SatOutput .and. global.SatFile) then
            !call write_tecplot_array(global.Sat,global.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit)
 	        call write_tecplot_array2(global.Sat,medium.SatNvar,global.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        
   
        if(medium.IceSatOutput .and. global.IceSatFile) then
            !call write_tecplot_array(global.IceSat,global.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit)
            call write_tecplot_array2(global.IceSat,medium.IceSatNvar,global.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        


        if(medium.VelFile .and. medium.VelOutput) then
	        !call write_tecplot_array(medium.Vx,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vy,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vz,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
            call write_tecplot_array2(medium.Vx,medium.VxNvar,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vy,medium.VyNvar,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vz,medium.VzNvar,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)

        end if


        if(medium.ConcOutput .and. global.ConcFile) then
	        do i=1,nspeciesmob
                if(BinaryMode) then
		            call write_tecplot_darray2(global.Conc(:,i:i),medium.ConcNvar(i),global.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit,medium.IDNum)
                else
	                write(medium.Unit,'(a)') '# '//trim(spname(i))
	                do j=1,global.NN
		                if(medium.UseN(j)) write(medium.Unit,'(5g26.17e3)') global.Conc(j,i:i)
                    end do
                end if
	        end do
        end if
        

        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
	        !call write_tecplot_darray(medium.Ap,medium.NE,medium.UseE,'# aperture (cell centred)',medium.DomainTruncate,medium.Unit)
 	        call write_tecplot_darray2(medium.Ap,medium.ApNvar,medium.NE,medium.UseE,'# aperture (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(BinaryMode) then
            call write_tecplot_iarray2(medium.FromPMLink,medium.Node3DNumNvar,global.NN,medium.UseN,'# 3D Node number ',medium.DomainTruncate,medium.Unit,medium.IDNum)
        else
            write(medium.Unit,'(a)') '# 3D node #' 
	        do i=1,global.NN
		        if(medium.UseN(i)) write(medium.Unit,'(5g26.17e3)') i
            end do
        end if

        if(BinaryMode) then
            if(allocated(nodemap32)) deallocate(nodemap32)
            allocate(nodemap32(jmax_M(medium.IDNum)*nln_padded),stat=ialloc)  ! jmax has been set to medium_ne_u  (i.e. used elements)
            call AllocChk(ialloc,'nodemap32 array')

            k=0
            do l=1,medium.NE
                if(medium.UseE(l)) then
                    select case (medium.NLN)
                    case (2)  ! line segment
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do

                    case (3) ! triangle, node 3 copied to node 4 (tecplot requires quadrilateral)
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                    case (4) ! if node 4 set to zero, is triangle and must copy node 3 to node 4
                        do j=1,3
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        if(medium.IN(4,l) == 0) then
                            nodemap32(k)=nodemap32(k-1)
                        else
                            nodemap32(k)=medium.TruncIndx(medium.IN(4,l))
                        end if
                    case (6) ! prism, must copy node 3 to node 4, node 6 to node 8
                        do j=1,3
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                        do j=4,6
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                    case (8)
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                    end select
                end if
            end do

            i = tecZoneNodeMapWrite32(OutputFileHandle_M(medium.IDNum), &
                outputZone, 0, 1, jmax_M(medium.IDNum)*nln_padded, nodeMap32)

        else
            write(medium.Unit,'(a)') '# element node lists'
            do l=1,medium.NE
                if(medium.UseE(l)) then
                    select case (medium.NLN)
                    case (2)
                        write(medium.Unit,'(2i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l))
                    case (3)
                        write(medium.Unit,'(4i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l))
                    case (4)
                        if(medium.IN(4,l) == 0) then
                            write(medium.Unit,'(4i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l))
                        else
                            write(medium.Unit,'(4i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(4,l))
                        end if
                    case (6)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l)), &
                                                     medium.TruncIndx(medium.IN(4,l)),medium.TruncIndx(medium.IN(5,l)),medium.TruncIndx(medium.IN(6,l)),medium.TruncIndx(medium.IN(6,l))
                    case (8)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(4,l)), &
                                                     medium.TruncIndx(medium.IN(5,l)),medium.TruncIndx(medium.IN(6,l)),medium.TruncIndx(medium.IN(7,l)),medium.TruncIndx(medium.IN(8,l))
                    end select
                end if
            end do
        end if
        

    end subroutine InitCommonTecplot
    !------------------------------------------------------------------------
    subroutine AppendCommonTecplot(medium,global)
         
        implicit none
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global
        integer :: i

        if(BinaryMode) then
            read(timestamp,*) SolutionTime
            StrandID=1

            ! always connect x,y,z,zone,fslice
            ShareVarFromZone_M(medium.XNvar,medium.IDNum)=1
            ShareVarFromZone_M(medium.YNvar,medium.IDNum)=1
            ShareVarFromZone_M(medium.ZNvar,medium.IDNum)=1
            ShareVarFromZone_M(medium.ZoneNvar,medium.IDNum)=1
            if(medium.Name == 'pm') ShareVarFromZone_M(medium.FsliceNvar,medium.IDNum)=1
            
        else

            if(double_xy) then
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)// ', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', DT=(DOUBLE DOUBLE), VARLOCATION=([',medium.ZoneNvar 
            else
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)// ', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', VARLOCATION=([',medium.ZoneNvar 
            end if
            if(medium.Name == 'pm') then
                dlist='1,2,3,4,5'           ! always connect x,y,z,zone,fslice
            else
                dlist='1,2,3,4'           ! always connect x,y,z,zone
            end if
        end if 

        if(medium.HeadOutput .and. .not. global.HeadFileN ) then
            if(BinaryMode) then
                ShareVarFromZone_M(medium.HeadNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PNvar,medium.IDNum)=1
            
            else
	            write(TMPStr,NFSFormat) medium.HeadNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        if(medium.SatOutput .and. .not. global.SatFileN ) then  
            if(BinaryMode) then
                ShareVarFromZone_M(medium.SatNvar,medium.IDNum)=1
            
            else
	            write(TMPStr,NFSFormat) medium.SatNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
            
        end if
    
        if(medium.IceSatOutput .and. .not. global.IceSatFileN) then
            if(BinaryMode) then
                ShareVarFromZone_M(medium.IceSatNvar,medium.IDNum)=1
            
            else
                write(TMPStr,NFSFormat) medium.IceSatNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        if(medium.VelFile .and. medium.VelOutput) call VelRead(medium)

        if(SpeciesFileExists .and. medium.ConcOutput .and. .not. global.ConcFileN) then
            do i=1,nspeciesmob
	            ! connect mobile concentrations
                if(BinaryMode) then
                    ShareVarFromZone_M(medium.ConcNvar(i),medium.IDNum)=1
                
                else
	                write(TMPStr,NFSFormat) medium.ConcNvar(i)
	                dlist=trim(dlist)//','//trim(TMPStr)
                end if    
            end do
        end if

        ! Always connect aperture
        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ApNvar,medium.IDNum)=1
            
            else
	            write(TMPStr,NFSFormat) medium.ApNvar
	            dlist=trim(dlist)//','//trim(TMPStr)

                write(temp_line,'(a,i7)') ',',medium.ApNvar
                zone_line=trim(zone_line)//temp_line
            end if
        end if

        ! Always connect 3D node numbers
        if(BinaryMode) then
            ShareVarFromZone_M(medium.Node3DNumNvar,medium.IDNum)=1
        else
	        write(TMPStr,NFSFormat) medium.Node3DNumNvar
	        dlist=trim(dlist)//','//trim(TMPStr)
        end if

        if(BinaryMode) then
            zoneTitle=trim(medium.name)//C_NULL_CHAR
            shareConnectivityFromZone=1

            i = tecZoneCreateFE(OutputFileHandle_M(medium.IDNum), zoneTitle, zoneType, &
                imax_M(medium.IDNum), jMax_M(medium.IDNum), varTypes_M(:,medium.IDNum), shareVarFromZone_M(:,medium.IDNum), &
                valueLocation_M(:,medium.IDNum), passiveVarList_M(:,medium.IDNum), &
                shareConnectivityFromZone, numFaceConnections, &
                faceNeighborMode, outputZone)
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecZoneCreateFE returned '//trim(TMPStr))
            else
                call Msg('tecZoneCreateFE success')
            end if

            i = tecZoneSetUnsteadyOptions(OutputFileHandle_M(medium.IDNum), &
                outputZone, solutionTime, strandID)

        else
        
            zone_line=trim(zone_line)//']=CELLCENTERED)'
            write(temp_line,'(3(a,i7),a)') ', VARSHARELIST=(['//trim(dlist)//'])'
            zone_line=trim(zone_line)//temp_line

            write(temp_line,'(3(a,i7),a)') ', CONNECTIVITYSHAREZONE=1'
            zone_line=trim(zone_line)//temp_line

            write(medium.Unit,'(a)') zone_line
        end if

        if(global.HeadFileN) then
            !call write_tecplot_darray(global.Head,global.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(global.Head,medium.HeadNvar,global.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        
        if(global.SatFileN) then
            !call write_tecplot_array(global.Sat,global.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(global.Sat,medium.SatNvar,global.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        
        if(global.IceSatFileN) then
            !call write_tecplot_array(global.IceSat,global.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit)
            call write_tecplot_array2(global.IceSat,medium.IceSatNvar,global.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        
        if(medium.VelFileN) then
	        !call write_tecplot_array(medium.Vx,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vy,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vz,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.Vx,medium.VxNvar,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vy,medium.VyNvar,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vz,medium.VzNvar,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(global.ConcFileN) then
	        do i=1,nspeciesmob
		        !call write_tecplot_darray(global.Conc(:,i:i),global.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit)
		        call write_tecplot_darray2(global.Conc(:,i:i),medium.ConcNvar(i),global.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if
    
        ! Always connect aperture and 3DNode# (see above )
    

    end subroutine AppendCommonTecplot
    !------------------------------------------------------------------------
    subroutine MultiTruncate(medium,global)
         
        implicit none
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global
        integer :: i, j
        allocate(medium.UseN(medium.NN),medium.UseE(medium.NE),medium.TruncIndx(global.NN),stat=status)
        if(status /= 0) then
	        call Msg('ALLOCATION ERROR: medium.UseN, medium.UseE, medium.TruncIndx')
	        stop
        end if
        medium.UseN(:) = .false.  ! flag which 3D nodes are used if truncating
        medium.UseE(:) = .false.  ! flag which tile elements are used if truncating
        medium.TruncIndx(:) = 0  ! list of new node #'s

        if(medium.DomainTruncate) then
	        do i=1,medium.NE   ! flag fracture nodes
		        sumx=0.0
		        sumy=0.0
		        sumz=0.0
		        nln_f=0
		        do j=1,medium.NLN
			        if(medium.IN(j,i) /= 0) then
				        sumx=sumx+global.X(medium.IN(j,i) )
				        sumy=sumy+global.Y(medium.IN(j,i) )
				        sumz=sumz+global.Z(medium.IN(j,i) )
				        nln_f=nln_f+1
			        end if
		        end do
		        xc=sumx/real(nln_f)
		        yc=sumy/real(nln_f)
		        zc=sumz/real(nln_f)
		        if(	xc < medium.XminTrunc .or. xc > medium.XmaxTrunc .or. &    ! don't use element if outside truncate limits
			        yc < medium.YminTrunc .or. yc > medium.YmaxTrunc .or. &
			        zc < medium.ZminTrunc .or. zc > medium.ZmaxTrunc         ) then

			        medium.UseE(i)=.false.
		        else
			        medium.UseE(i)=.true.
			        do j=1,medium.NLN
		                if(medium.name =='pm') then
		                    if(medium.IN(j,i)>0) medium.UseN(medium.IN(j,i))=.true.
		                else
		                    if(medium.IN(j,i)>0) medium.UseN(medium.FromPmLink(medium.IN(j,i))-medium.StartIndex)=.true.
		                end if
			        end do
		        end if
	        end do

            medium.NE_u=0   ! count elements used
	        do i=1,medium.NE
		        if(medium.UseE(i)) medium.NE_u=medium.NE_u+1
	        end do
	        medium.NN_u=0   ! form truncation index
	        do i=1,medium.NN
		        if(medium.UseN(i)) then
			        medium.NN_u=medium.NN_u+1
			        ! old node i is now node medium.NN_u
		            if(medium.Name == 'pm') then 
			            medium.TruncIndx(i)=medium.NN_u
		            else
			            medium.TruncIndx(medium.ToPmLink(i))=medium.NN_u
			        end if
		        end if
	        end do
	        call Msg(' ')
	        call Msg(trim(medium.name)//' truncated mesh size:')
            write(TMPStr,*) medium.NN_u
	        call Msg('Nodes:       '//trim(TMPStr))
            write(TMPStr,*) medium.NE_u
	        call Msg('Elements:    '//trim(TMPStr))

        else
	        ! truncate domain to use only elements and node data
	        medium.UseE(:)=.true.
	        medium.NE_u=medium.NE
	        medium.UseN(:)=.false.
	        do i=1,medium.NE   
		        do j=1,medium.NLN
		            if(medium.name =='pm') then
		                if(medium.IN(j,i)>0) medium.UseN(medium.IN(j,i))=.true.
		            else
		                if(medium.IN(j,i)>0) medium.UseN(medium.FromPmLink(medium.IN(j,i))-medium.StartIndex)=.true.
		            end if
		        end do
	        end do

	        medium.NN_u=0   ! form truncation index
	        do i=1,medium.NN
		        if(medium.UseN(i)) then
			        medium.NN_u=medium.NN_u+1
			        ! old node i is now node medium.NN_u
		            if(medium.Name == 'pm') then 
			            medium.TruncIndx(i)=medium.NN_u
		            else
			            medium.TruncIndx(medium.ToPmLink(i))=medium.NN_u
			        end if
		        end if
	        end do
	        call Msg(' ')
	        call Msg(trim(medium.name)//' full mesh size:')
            write(TMPStr,*) medium.NN_u
	        call Msg(' Nodes:       '//trim(TMPStr))
            write(TMPStr,*) medium.NE_u
	        call Msg(' Elements:    '//trim(TMPStr))
        end if

    end subroutine MultiTruncate
    !------------------------------------------------------------------------
    subroutine InitMultiTecplot(medium,global)
          
        implicit none
        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global
        integer :: i, j, k, l

        if(BinaryMode) then 
            if(Fileformat==1) then
                OutputFileName=trim(LocalPrefix)//'o.'//trim(medium.name)//'.szplt'// C_NULL_CHAR
            else if (Fileformat==0) then
                OutputFileName=trim(LocalPrefix)//'o.'//trim(medium.name)//'.plt'// C_NULL_CHAR
            end if
            
        else
            Outputfilename=trim(LocalPrefix)//'o.'//trim(medium.name)//'.dat'
            call OpenAscii(medium.Unit,Outputfilename)
            write(medium.Unit,'(a)') 'TITLE = "'//title//'"'
        end if
    
        call Msg(' ')
        call Msg(trim(medium.name)//' data to tecplot')
        call Msg('Creating tecplot file '//OutputFileName)

        medium.Nvar=0
        call TecplotVarAdd('X',medium.Nvar,medium.XNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Y',medium.Nvar,medium.YNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Z',medium.Nvar,medium.ZNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=2 ! double precision
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0
        
        call TecplotVarAdd('Zone '//trim(medium.name),medium.Nvar,medium.ZoneNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=3 ! integer 
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=0 ! cell-centred
        TBinary_shareVarFromZone(medium.Nvar)=0

    !
        if(BinaryMode) then
            continue

        else    
            if(double_xy) then
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)//', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', DT=(DOUBLE DOUBLE), VARLOCATION=([',medium.ZoneNvar 
            else
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)//', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', VARLOCATION=([',medium.ZoneNvar 
            end if
        end if
        
        if(medium.Name == 'pm') call FsliceInit(medium)


        if(medium.HeadOutput) call HeadInit(medium)
        
    
        if(medium.DeltaZOutput) call DeltaZInit(medium)
        if((medium.Name == 'pm' .or. medium.Name == 'dual' .or. medium.Name == 'frac') .and. medium.SatOutput) call SatInit(medium)

        !Depth to Groundwater Table
	    if( medium.Name == 'pm' .and. medium.Depth2GWTOutput) call Depth2GWTInit(medium)

        if(medium.Name == 'pm'.and. medium.IceSatOutput) call IceSatInit(medium)

        if(medium.VelOutput) call VelInit(medium)

        if(medium.PecletOutput) call PecletInit(medium)
        if(medium.DiffPecletOutput) call DiffPecletInit(medium)
        if(medium.ElemKOutput) call ElemKInit(medium)
        if(medium.TVKOutput) call TVKInit(medium)
        if(medium.ElemPorOutput) call ElemPorInit(medium)
        if(medium.ElemStorOutput) call ElemStorInit(medium)
        if(medium.ElemTortOutput) call ElemTortInit(medium)
        if(medium.ElemIbedFractionOutput) call ElemIbedFractionInit(medium)

        if(medium.PermafrostOutput) call PermafrostInit(medium)

        if(medium.Name == 'olf' .and. medium.ETOutput) call ETInit(medium)
        if(medium.ExchFluxOutput) call ExchFluxInit(medium)
        if(SpeciesFileExists) then
	        if(medium.ConcOutput) call ConcInit(medium)
	        if(medium.ExchSolOutput) then
	            call ExchSolAdvInit(medium)
	            call ExchSolDispInit(medium)
	        end if
        end if

        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
            call TecplotVarAdd('Aperture',medium.Nvar,medium.ApNvar,medium.VarList)
            TBinary_vartype(medium.Nvar)=2 ! double precision
            TBinary_passiveVarList(medium.Nvar)=0
            TBinary_valueLocation(medium.Nvar)=0 ! cell-centred
            TBinary_shareVarFromZone(medium.Nvar)=0

            if(.not. BinaryMode) then
                write(temp_line,'(a,i7)') ',',medium.ApNvar
                zone_line=trim(zone_line)//temp_line
            end if
        end if
    
        if(medium.Name /= 'pm') then
            call TecplotVarAdd('3DNode#',medium.Nvar,medium.Node3DNumNvar,medium.VarList)
            TBinary_vartype(medium.Nvar)=1 ! single precision
            TBinary_passiveVarList(medium.Nvar)=0
            TBinary_valueLocation(medium.Nvar)=1 ! node-centred
            TBinary_shareVarFromZone(medium.Nvar)=0
        end if

        if(BinaryMode) then
            dataSetTitle=trim(title(1))//C_NULL_CHAR
            varnames=trim(medium.VarList)//C_NULL_CHAR
            
            read(timestamp,*) SolutionTime
            StrandID=1

            
            fileType=0

            i = tecFileWriterOpen(OutputFileName, dataSetTitle, varNames, &
                fileFormat, fileType, 1, C_NULL_PTR, OutputFileHandle_M(medium.IDNum))
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecFileWriterOpen returned '//trim(TMPStr))
            else
                call Msg('tecFileWriterOpen success')
            end if
            i = tecFileSetDiagnosticsLevel(OutputFileHandle_M(medium.IDNum), outputDebugInfo)
             if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecFileSetDiagnosticsLevel returned '//trim(TMPStr))
            else
                call Msg('tecFileSetDiagnosticsLevel success')
            end if
           
            zoneTitle=trim(medium.name)//C_NULL_CHAR
            select case (medium.nln)
            case (2)  ! line
                nln_padded=2
                Zonetype=1
            case (4)  ! tetrahedra or 4-node quadrilateral
                nln_padded=4
                if(medium.name == 'pm' .or. medium.name == 'dual') then   ! 3D element so FETETRAHEDRON
                    Zonetype=4
                else  ! 2D element so FEQUADRILATERAL
                    Zonetype=3
                end if
            case (6)  ! prisms
                nln_padded=8
                Zonetype=5
            case (8)  ! blocks
                nln_padded=8
                Zonetype=5
            case default
                write(TMPStr,*) medium.nln
                call ErrMsg('nln not recognized: '//trim(TMPStr))
            end select    
            imax_M(medium.IDNum)=medium.nn_u
            jmax_M(medium.IDNum)=medium.ne_u
            
            do i=1,medium.Nvar
                VarTypes_M(i,medium.IDNum)=TBinary_VarType(i)
                passiveVarList_M(i,medium.IDNum)=TBinary_passiveVarList(i)
                valueLocation_M(i,medium.IDNum)=TBinary_valueLocation(i)
                shareVarFromZone_M(i,medium.IDNum)=TBinary_shareVarFromZone(i)
            end do
            
            numFaceConnections=0
            faceNeighborMode=0
            shareConnectivityFromZone=0

            i = tecZoneCreateFE(OutputFileHandle_M(medium.IDNum), zoneTitle, zoneType, &
                imax_M(medium.IDNum), jMax_M(medium.IDNum), varTypes_M(:,medium.IDNum), shareVarFromZone_M(:,medium.IDNum), &
                valueLocation_M(:,medium.IDNum), passiveVarList_M(:,medium.IDNum), &
                shareConnectivityFromZone, numFaceConnections, &
                faceNeighborMode, outputZone)
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecZoneCreateFE returned '//trim(TMPStr))
            else
                call Msg('tecZoneCreateFE success')
            end if

           
            i = tecZoneSetUnsteadyOptions(OutputFileHandle_M(medium.IDNum), &
                outputZone, solutionTime, strandID)
    
            
            call BuildCustomZoneNames(medium,global)
            i = tecCustomLabelsAddSet(outputFileHandle_M(medium.IDNum), labelSet_M(medium.IDNum))
 
        else
            write(medium.Unit,'(a)') trim(medium.VarList)
            zone_line=trim(zone_line)//']=CELLCENTERED)'
            write(medium.Unit,'(a)') zone_line
        end if

        call write_tecplot_darray2(medium.X,medium.XNvar,medium.NN,medium.UseN,'# x',medium.DomainTruncate,medium.Unit,medium.IDNum)
        call write_tecplot_darray2(medium.Y,medium.YNvar,medium.NN,medium.UseN,'# y',medium.DomainTruncate,medium.Unit,medium.IDNum)
        call write_tecplot_darray2(medium.Z,medium.ZNvar,medium.NN,medium.UseN,'# z',medium.DomainTruncate,medium.Unit,medium.IDNum)
        call write_tecplot_iarray2(medium.ZoneNum,medium.ZoneNvar,medium.NE,medium.UseE,'# zone (cell centred',medium.DomainTruncate,medium.Unit,medium.IDNum)
        if(medium.Name == 'pm') call write_tecplot_iarray2(medium.Fslice,medium.FsliceNvar,medium.NN,medium.UseN,'# feflow slice',medium.DomainTruncate,medium.Unit,medium.IDNum)
    
        if(medium.HeadFile .and. medium.HeadOutput) then
	        call write_tecplot_darray2(medium.Head,medium.HeadNvar,medium.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit,medium.IDNum)
            if(medium.Name == 'pm') then
	            call write_tecplot_darray2(medium.P,medium.PNvar,medium.NN,medium.UseN,'# P',medium.DomainTruncate,medium.Unit,medium.IDNum)
            end if
	        if(medium.Name == 'olf' .or. medium.Name == 'chan') then
	            call write_tecplot_array2(medium.Depth,medium.DepthNvar,medium.NN,medium.UseN,'# depth',medium.DomainTruncate,medium.Unit,medium.IDNum)
	            call write_tecplot_array2(medium.LogDepth,medium.LogDepthNvar,medium.NN,medium.UseN,'# log10Depth)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end if
        end if
        
        if((medium.Name == 'pm' .or. medium.Name == 'dual' .or. medium.Name == 'olf') .and. medium.SatFile .and. medium.SatOutput) then
	        call write_tecplot_array2(medium.Sat,medium.SatNvar,medium.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

	    if( medium.Name == 'pm' .and. medium.HeadFile .and. medium.Depth2GWTOutput) then
	        call write_tecplot_array2(medium.Depth2GWT,medium.Depth2GWTNvar,medium.NN,medium.UseN,'# Depth2GWT',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
    
    
        if(medium.Name == 'pm' .and. medium.IceSatFile .and. medium.IceSatOutput) then
            call write_tecplot_array2(medium.IceSat,medium.IceSatNvar,medium.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.VelFile .and. medium.VelOutput) then
	        call write_tecplot_array2(medium.Vx,medium.VxNvar,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vy,medium.VyNvar,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vz,medium.VzNvar,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.PecletFile .and. medium.PecletOutput) then
	        call write_tecplot_array2(medium.Pecletx,medium.PecletxNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Peclety,medium.PecletyNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Pecletz,medium.PecletzNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.DiffPecletFile .and. medium.DiffPecletOutput) then
	        call write_tecplot_array2(medium.DiffPecletx,medium.DiffPecletxNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.DiffPeclety,medium.DiffPecletyNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.DiffPecletz,medium.DiffPecletzNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ElemKFile .and. medium.ElemKOutput) then
	        call write_tecplot_darray2(medium.ElemKx,medium.ElemKxNvar,medium.NE,medium.UseE,'# Kxx (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ElemKy,medium.ElemKyNvar,medium.NE,medium.UseE,'# Kyy (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ElemKz,medium.ElemKzNvar,medium.NE,medium.UseE,'# Kzz (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.TVKFile .and. medium.TVKOutput) then
	        call write_tecplot_array2(medium.TVKx,medium.TVKxNvar,medium.NE,medium.UseE,'# Time-varying Kxx (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.TVKy,medium.TVKyNvar,medium.NE,medium.UseE,'# Time-varying Kyy (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.TVKz,medium.TVKzNvar,medium.NE,medium.UseE,'# Time-varying Kzz (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemPorFile .and. medium.ElemPorOutput) then
	        call write_tecplot_array2(medium.ElemPor,medium.ElemPorNvar,medium.NE,medium.UseE,'# Porosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemStorFile .and. medium.ElemStorOutput) then
	        call write_tecplot_darray2(medium.ElemStor,medium.ElemStorNvar,medium.NE,medium.UseE,'# Specific storage (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemTortFile .and. medium.ElemTortOutput) then
	        call write_tecplot_darray2(medium.ElemTort,medium.ElemTortNvar,medium.NE,medium.UseE,'# Tortuosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemIbedFractionFile .and. medium.ElemIbedFractionOutput) then
	        call write_tecplot_darray2(medium.ElemIbedFraction,medium.ElemIbedFractionNvar,medium.NE,medium.UseE,'# IbedFractionosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.PermafrostFile .and. medium.PermafrostOutput) then
	        call write_tecplot_array2(medium.Permafrost,medium.PermafrostNvar,medium.NE,medium.UseE,'# Permafrost (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.Name == 'olf' .and. medium.ETFile .and. medium.ETOutput) then
	        call write_tecplot_darray2(medium.Evap,medium.EvapNvar,medium.NN,medium.UseN,'# surface water evapopration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.PmEvap,medium.PmEvapNvar,medium.NN,medium.UseN,'# subsurface evapopration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.PmTranspire,medium.PmTranspireNvar,medium.NN,medium.UseN,'# subsurface transpiration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ETTotal,medium.ETTotalNvar,medium.NN,medium.UseN,'# Total ET',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ExchFluxFile .and.medium.ExchFluxOutput) then
	        call write_tecplot_array2(medium.ExchFlux,medium.ExchFluxNvar,medium.NN,medium.UseN,'# exchange flux',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ConcFile .and. medium.ConcOutput) then
	        do i=1,nspeciesmob
		        call write_tecplot_darray2(medium.Conc(:,i:i),medium.ConcNvar(i),medium.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if
        if(medium.ExchSolAdvFile .and. medium.ExchSolOutput) then
	        do i=1,nspeciesmob
		        call write_tecplot_array2(medium.ExchSolAdv(:,i:i),medium.ExchSolAdvNvar(i),medium.NN,medium.UseN,'# adv. solute exchange',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if
        if(medium.ExchSolDispFile .and. medium.ExchSolOutput) then
	        do i=1,nspeciesmob
		        call write_tecplot_array2(medium.ExchSolDisp(:,i:i),medium.ExchSolDispNvar(i),medium.NN,medium.UseN,'# disp. solute exchange',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if

        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
	        call write_tecplot_darray2(medium.Ap,medium.ApNvar,medium.NE,medium.UseE,'# aperture (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.Name /= 'pm') then
            if(BinaryMode) then
                call write_tecplot_iarray2(medium.ToPMLink,medium.Node3DNumNvar,medium.NN,medium.UseN,'# 3D Node number ',medium.DomainTruncate,medium.Unit,medium.IDNum)
            else
                write(medium.Unit,'(a)') '# 3D node #' 
	            do i=1,medium.NN
		            if(medium.UseN(i)) write(medium.Unit,'(5g26.17e3)') medium.ToPmLink(i)
                end do
            end if
        end if

        continue

        
        if(BinaryMode) then
            if(allocated(nodemap32)) deallocate(nodemap32)
            allocate(nodemap32(jmax_M(medium.IDNum)*nln_padded),stat=ialloc)  ! jmax has been set to medium_ne_u  (i.e. used elements)
            call AllocChk(ialloc,'nodemap32 array')

            k=0
            do l=1,medium.NE
                if(medium.UseE(l)) then
                    select case (medium.NLN)
                    case (2)  ! line segment
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do

                    case (3) ! triangle, node 3 copied to node 4 (tecplot requires quadrilateral)
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                    case (4) ! if node 4 set to zero, is triangle and must copy node 3 to node 4
                        do j=1,3
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        if(medium.IN(4,l) == 0) then
                            nodemap32(k)=nodemap32(k-1)
                        else
                            nodemap32(k)=medium.TruncIndx(medium.IN(4,l))
                        end if
                    case (6) ! prism, must copy node 3 to node 4, node 6 to node 8
                        do j=1,3
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                        do j=4,6
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                        k=k+1
                        nodemap32(k)=nodemap32(k-1)
                    case (8)
                        do j=1,medium.NLN
                            k=k+1
                            nodemap32(k)=medium.TruncIndx(medium.IN(j,l))
                        end do
                    end select
                end if
            end do

            i = tecZoneNodeMapWrite32(OutputFileHandle_M(medium.IDNum), &
                outputZone, 0, 1, jmax_M(medium.IDNum)*nln_padded, nodeMap32)

        else
            write(medium.Unit,'(a)') '# element node lists'
            do l=1,medium.NE
                if(medium.UseE(l)) then
                    select case (medium.NLN)
                    case (2)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l))
                    case (3)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l))
                    case (4)
                        if(medium.IN(4,l) == 0) then
                            write(medium.Unit,'(4i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l))
                        else
                            write(medium.Unit,'(4i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(4,l))
                        end if
                    case (6)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(3,l)), &
                                                     medium.TruncIndx(medium.IN(4,l)),medium.TruncIndx(medium.IN(5,l)),medium.TruncIndx(medium.IN(6,l)),medium.TruncIndx(medium.IN(6,l))
                    case (8)
                        write(medium.Unit,'(8i8,1x)') medium.TruncIndx(medium.IN(1,l)),medium.TruncIndx(medium.IN(2,l)),medium.TruncIndx(medium.IN(3,l)),medium.TruncIndx(medium.IN(4,l)), &
                                                     medium.TruncIndx(medium.IN(5,l)),medium.TruncIndx(medium.IN(6,l)),medium.TruncIndx(medium.IN(7,l)),medium.TruncIndx(medium.IN(8,l))
                    end select
                end if
            end do
        end if


    end subroutine InitMultiTecplot
    !------------------------------------------------------------------------
    subroutine AppendMultiTecplot(medium)
              
        implicit none
        integer :: i 
        type(HGSTecplotdomain) medium


        if(BinaryMode) then
            read(timestamp,*) SolutionTime
            StrandID=1

            ! always connect x,y
            ShareVarFromZone_M(medium.XNvar,medium.IDNum)=1
            ShareVarFromZone_M(medium.YNvar,medium.IDNum)=1
            
        else

            if(double_xy) then
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)// ', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', DT=(DOUBLE DOUBLE), VARLOCATION=([',medium.ZoneNvar 
            else
                write(zone_line,'(3(a,i9))') 'ZONE  T="'//trim(medium.name)//'", SOLUTIONTIME='//trim(TimeStamp)// ', &
                DATAPACKING=BLOCK, N=',medium.NN_u,', E=',medium.NE_u,&
                ', ZONETYPE='//medium.ZoneType//', VARLOCATION=([',medium.ZoneNvar 
            end if

            dlist='1,2,'           ! always connect x,y
        end if

        if(medium.DeltaZFile .and. medium.DeltaZOutput) then
            call DeltaZRead(medium)
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ZoneNvar,medium.IDNum)=1
                if(medium.Name == 'pm') ShareVarFromZone_M(medium.FsliceNvar,medium.IDNum)=1
            else
                if(medium.Name == 'pm') then
                    dlist=trim(dlist)//'4,5'  ! always connect zone,fslice
                else
                    dlist=trim(dlist)//'4'  ! always connect zone
                end if
            end if
        else
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ZNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.ZoneNvar,medium.IDNum)=1
                if(medium.Name == 'pm') ShareVarFromZone_M(medium.FsliceNvar,medium.IDNum)=1
            else
                if(medium.Name == 'pm') then
                    dlist=trim(dlist)//'3,4,5'  ! always connect zone, fslice
                else
                    dlist=trim(dlist)//'3,4'  ! always connect zone
                end if
            end if
        end if


        if(medium.HeadFile .and. medium.HeadOutput) call HeadRead(medium)
        if((medium.Name == 'pm' .or. medium.Name == 'dual' .or. medium.Name == 'olf') .and. medium.SatFile .and. medium.SatOutput) call SatRead(medium)
        if((medium.Name == 'pm') .and. medium.HeadFile .and. medium.Depth2GWTOutput) call Depth2GWTRead(medium)

        if(medium.Name == 'pm' .and. medium.IceSatFile .and. medium.IceSatOutput) call IceSatRead(medium)
        if(medium.VelFile .and. medium.VelOutput) call VelRead(medium)
    
        if(medium.PecletFile .and. medium.PecletOutput) call PecletRead(medium)
        if(medium.DiffPecletFile .and. medium.DiffPecletOutput) call DiffPecletRead(medium)
        if(medium.ElemKFile .and. medium.ElemKOutput) call ElemKRead(medium)
        if(medium.TVKFile .and. medium.TVKOutput) call TVKRead(medium)
        if(medium.ElemPorFile .and. medium.ElemPorOutput) call ElemPorRead(medium)
        if(medium.ElemStorFile .and. medium.ElemStorOutput) call ElemStorRead(medium)
        if(medium.ElemTortFile .and. medium.ElemTortOutput) call ElemTortRead(medium)
        if(medium.ElemIbedFractionFile .and. medium.ElemIbedFractionOutput) call ElemIbedFractionRead(medium)

        if(medium.PermafrostFile .and. medium.PermafrostOutput) call PermafrostRead(medium)
    
        if(medium.ETFile .and. medium.ETOutput) call ETRead(medium)
        if(medium.ExchFluxFile .and.medium.ExchFluxOutput) call ExchFluxRead(medium)
        if(SpeciesFileExists) then
	        if(medium.ConcFile .and. medium.ConcOutput) call ConcRead(medium)
	        if(medium.ExchSolAdvFile .and. medium.ExchSolOutput) then
	            call ExchSolAdvRead(medium)
	            call ExchSolDispRead(medium)
	        end if
        end if

        ! Connect aperture 
        if(medium.Name == 'frac' .and. medium.ApertureOutput) then
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ApNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ApNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if

            write(temp_line,'(a,i7)') ',',medium.ApNvar
            zone_line=trim(zone_line)//temp_line
	    end if

        ! Connect 3D node numbers if not pm
        if(medium.Name /= 'pm') then
            if(BinaryMode) then
                ShareVarFromZone_M(medium.Node3DNumNvar,medium.IDNum)=1
            else
	            write(TMPStr,NFSFormat) medium.Node3DNumNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        if(BinaryMode) then
            zoneTitle=trim(medium.name)//C_NULL_CHAR
            shareConnectivityFromZone=1
            
            select case (medium.nln)
            case (2)  ! line
                nln_padded=2
                Zonetype=1
            case (4)  ! tetrahedra or 4-node quadrilateral
                nln_padded=4
                if(medium.name == 'pm' .or. medium.name == 'dual') then   ! 3D element so FETETRAHEDRON
                    Zonetype=4
                else  ! 2D element so FEQUADRILATERAL
                    Zonetype=3
                end if
            case (6)  ! prisms
                nln_padded=8
                Zonetype=5
            case (8)  ! blocks
                nln_padded=8
                Zonetype=5
            case default
                write(TMPStr,*) medium.nln
                call ErrMsg('nln not recognized: '//trim(TMPStr))
            end select    


            i = tecZoneCreateFE(OutputFileHandle_M(medium.IDNum), zoneTitle, zoneType, &
                imax_M(medium.IDNum), jMax_M(medium.IDNum), varTypes_M(:,medium.IDNum), shareVarFromZone_M(:,medium.IDNum), &
                valueLocation_M(:,medium.IDNum), passiveVarList_M(:,medium.IDNum), &
                shareConnectivityFromZone, numFaceConnections, &
                faceNeighborMode, outputZone)
            i = tecZoneSetUnsteadyOptions(OutputFileHandle_M(medium.IDNum), &
                outputZone, solutionTime, strandID)



            
            if(i/=0) then
                write(TMPStr,*) i
                call Msg('tecZoneCreateFE returned '//trim(TMPStr))
            else
                call Msg('tecZoneCreateFE success')
            end if
        else
            zone_line=trim(zone_line)//']=CELLCENTERED)'
            write(temp_line,'(3(a,i7),a)') ', VARSHARELIST=(['//trim(dlist)//'])'
            zone_line=trim(zone_line)//temp_line
            write(temp_line,'(3(a,i7),a)') ', CONNECTIVITYSHAREZONE=1'
            zone_line=trim(zone_line)//temp_line
            write(medium.Unit,'(a)') zone_line
        end if

        if(medium.DeltaZFileN) then
	        !call write_tecplot_darray(medium.Z,medium.NN,medium.UseN,'# Z',medium.DomainTruncate,medium.Unit)
            call write_tecplot_darray2(medium.Z,medium.ZNvar,medium.NN,medium.UseN,'# z',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.HeadFileN) then
	        call write_tecplot_darray2(medium.Head,medium.HeadNvar,medium.NN,medium.UseN,'# head',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        if(medium.Name == 'pm') then
	            call write_tecplot_darray2(medium.P,medium.PNvar,medium.NN,medium.UseN,'# P',medium.DomainTruncate,medium.Unit,medium.IDNum)
            end if
	        if(medium.Name == 'olf' .or. medium.Name == 'chan') then
	            call write_tecplot_array2(medium.Depth,medium.DepthNvar,medium.NN,medium.UseN,'# depth',medium.DomainTruncate,medium.Unit,medium.IDNum)
	            call write_tecplot_array2(medium.LogDepth,medium.LogDepthNvar,medium.NN,medium.UseN,'# log10Depth)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end if
        end if

        if(medium.SatFileN) then
	        !call write_tecplot_array(medium.Sat,medium.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.Sat,medium.SatNvar,medium.NN,medium.UseN,'# saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

	    if( medium.Name == 'pm' .and. medium.Depth2GWTOutput .and. medium.HeadFileN) then
	        !call write_tecplot_array(medium.Depth2GWT,medium.NN,medium.UseN,'# Depth2GWT',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.Depth2GWT,medium.Depth2GWTNvar,medium.NN,medium.UseN,'# Depth2GWT',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
    
        if(medium.IceSatFileN) then
	        !call write_tecplot_array(medium.IceSat,medium.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit)
            call write_tecplot_array2(medium.IceSat,medium.IceSatNvar,medium.NN,medium.UseN,'# ice saturation',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.VelFileN) then
	        !call write_tecplot_array(medium.Vx,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vy,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Vz,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.Vx,medium.VxNvar,medium.NE,medium.UseE,'# x linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vy,medium.VyNvar,medium.NE,medium.UseE,'# y linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Vz,medium.VzNvar,medium.NE,medium.UseE,'# z linear velocity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.PecletFileN .and. medium.PecletOutput) then
	        !call write_tecplot_array(medium.Pecletx,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Peclety,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.Pecletz,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.Pecletx,medium.PecletxNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Peclety,medium.PecletyNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Pecletz,medium.PecletzNvar,medium.NE,medium.UseE,'# Peclet (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.DiffPecletFileN .and. medium.DiffPecletOutput) then
	        !call write_tecplot_array(medium.DiffPecletx,medium.NE,medium.UseE,'# Diffusion Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.DiffPeclety,medium.NE,medium.UseE,'# Diffusion Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_array(medium.DiffPecletz,medium.NE,medium.UseE,'# Diffusion Peclet (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.DiffPecletx,medium.DiffPecletxNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.DiffPeclety,medium.DiffPecletyNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.DiffPecletz,medium.DiffPecletzNvar,medium.NE,medium.UseE,'# Diffusion Peclet  (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemKFileN .and. medium.ElemKOutput) then
	        !call write_tecplot_darray(medium.ElemKx,medium.NE,medium.UseE,'# Kxx (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_darray(medium.ElemKy,medium.NE,medium.UseE,'# Kyy (cell centred)',medium.DomainTruncate,medium.Unit)
	        !call write_tecplot_darray(medium.ElemKz,medium.NE,medium.UseE,'# Kzz (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(medium.ElemKx,medium.ElemKxNvar,medium.NE,medium.UseE,'# Kxx (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ElemKy,medium.ElemKyNvar,medium.NE,medium.UseE,'# Kyy (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ElemKz,medium.ElemKzNvar,medium.NE,medium.UseE,'# Kzz (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.TVKFileN .and. medium.TVKOutput) then
	        call write_tecplot_array2(medium.TVKx,medium.TVKxNvar,medium.NE,medium.UseE,'# Time-varying Kxx (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.TVKy,medium.TVKyNvar,medium.NE,medium.UseE,'# Time-varying Kyy (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.TVKz,medium.TVKzNvar,medium.NE,medium.UseE,'# Time-varying Kzz (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ElemPorFileN .and. medium.ElemPorOutput) then
	        !call write_tecplot_array(medium.ElemPor,medium.NE,medium.UseE,'# Porosity (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.ElemPor,medium.ElemPorNvar,medium.NE,medium.UseE,'# Porosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemStorFileN .and. medium.ElemStorOutput) then
	        !call write_tecplot_darray(medium.ElemStor,medium.NE,medium.UseE,'# Specific storage (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(medium.ElemStor,medium.ElemStorNvar,medium.NE,medium.UseE,'# Specific storage (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemTortFileN .and. medium.ElemTortOutput) then
	        !call write_tecplot_darray(medium.ElemTort,medium.NE,medium.UseE,'# Tortuosity (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(medium.ElemTort,medium.ElemTortNvar,medium.NE,medium.UseE,'# Tortuosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if
        if(medium.ElemIbedFractionFileN .and. medium.ElemIbedFractionOutput) then
	        !call write_tecplot_darray(medium.ElemIbedFraction,medium.NE,medium.UseE,'# ElemIbedFraction (cell centred)',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(medium.ElemIbedFraction,medium.ElemIbedFractionNvar,medium.NE,medium.UseE,'# IbedFractionosity (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.PermafrostFileN .and. medium.PermafrostOutput) then 
	        call write_tecplot_array2(medium.Permafrost,medium.PermafrostNvar,medium.NE,medium.UseE,'# Permafrost (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_array2(medium.Permafrost,medium.PermafrostNvar,medium.NE,medium.UseE,'# Permafrost (cell centred)',medium.DomainTruncate,medium.Unit,medium.IDNum)
            !call PermafrostRead(medium)
        end if

        if(medium.ETFileN .and. medium.ETOutput) then
            !call write_tecplot_darray(medium.Evap,medium.NN,medium.UseN,'# surface water evapopration',medium.DomainTruncate,medium.Unit)
            !call write_tecplot_darray(medium.PmEvap,medium.NN,medium.UseN,'# subsurface evapopration',medium.DomainTruncate,medium.Unit)
            !call write_tecplot_darray(medium.PmTranspire,medium.NN,medium.UseN,'# subsurface transpiration',medium.DomainTruncate,medium.Unit)
            !call write_tecplot_darray(medium.ETTotal,medium.NN,medium.UseN,'# Total ET',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_darray2(medium.Evap,medium.EvapNvar,medium.NN,medium.UseN,'# surface water evapopration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.PmEvap,medium.PmEvapNvar,medium.NN,medium.UseN,'# subsurface evapopration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.PmTranspire,medium.PmTranspireNvar,medium.NN,medium.UseN,'# subsurface transpiration',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        call write_tecplot_darray2(medium.ETTotal,medium.ETTotalNvar,medium.NN,medium.UseN,'# Total ET',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ExchFluxFileN .and.medium.ExchFluxOutput) then
	        !call write_tecplot_array(medium.ExchFlux,medium.NN,medium.UseN,'# exchange flux',medium.DomainTruncate,medium.Unit)
	        call write_tecplot_array2(medium.ExchFlux,medium.ExchFluxNvar,medium.NN,medium.UseN,'# exchange flux',medium.DomainTruncate,medium.Unit,medium.IDNum)
        end if

        if(medium.ConcFileN) then
	        do i=1,nspeciesmob
		        !call write_tecplot_darray(medium.Conc(:,i:i),medium.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit)
		        call write_tecplot_darray2(medium.Conc(:,i:i),medium.ConcNvar(i),medium.NN,medium.UseN,'# '//trim(spname(i)),medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if

        if(medium.ExchSolAdvFileN .and. medium.ExchSolOutput) then
	        do i=1,nspeciesmob
		        !call write_tecplot_array(medium.ExchSolAdv(:,i:i),medium.NN,medium.UseN,'# adv. solute exchange',medium.DomainTruncate,medium.Unit)
		        call write_tecplot_array2(medium.ExchSolAdv(:,i:i),medium.ExchSolAdvNvar(i),medium.NN,medium.UseN,'# adv. solute exchange',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if
        if(medium.ExchSolDispFileN .and. medium.ExchSolOutput) then
	        do i=1,nspeciesmob
		        !call write_tecplot_array(medium.ExchSolDisp(:,i:i),medium.NN,medium.UseN,'# disp. solute exchange',medium.DomainTruncate,medium.Unit)
		        call write_tecplot_array2(medium.ExchSolDisp(:,i:i),medium.ExchSolDispNvar(i),medium.NN,medium.UseN,'# disp. solute exchange',medium.DomainTruncate,medium.Unit,medium.IDNum)
	        end do
        end if

    end subroutine AppendMultiTecplot
    !------------------------------------------------------------------------
    subroutine HeadInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i

        filename=trim(LocalPrefix)//'o.head_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.HeadFile)
        if(medium.HeadFile) then
            allocate(medium.Head(medium.NN),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate array head')
	            stop
            end if
            medium.Head(:) = 0 
            call init_var8(medium.NN,filename,medium.Head,'Head',medium.HeadNvar,medium.Nvar,medium.VarList)
            if(medium.Name == 'pm') then
                allocate(medium.P(medium.NN),stat=status)
                if(status /= 0) then
	                call Msg(' Failed to allocate P')
	                stop
                end if
                call TecplotVarAdd('P',medium.Nvar,medium.PNvar,medium.VarList)
                TBinary_vartype(medium.PNvar)=2  ! single precision
                TBinary_passiveVarList(medium.PNvar)=0
                TBinary_valueLocation(medium.PNvar)=1 ! node-centred
                TBinary_shareVarFromZone(medium.PNvar)=0
                do i=1,medium.NN
		            if(.not. yVertical) then
                        medium.P(i)=medium.Head(i)-medium.Z(i)
                    else
                        medium.P(i)=medium.Head(i)-medium.Y(i)
                    end if
                end do
            end if


            if(medium.Name == 'olf' .or. medium.Name == 'chan') then
                allocate(medium.Depth(medium.NN),medium.LogDepth(medium.NN),stat=status)
                if(status /= 0) then
	                call Msg(' Failed to allocate Depth, LogDepth')
	                stop
                end if
                call TecplotVarAdd('Depth',medium.Nvar,medium.DepthNvar,medium.VarList)
                TBinary_vartype(medium.DepthNvar)=1  ! single precision
                TBinary_passiveVarList(medium.DepthNvar)=0
                TBinary_valueLocation(medium.DepthNvar)=1 ! node-centred
                TBinary_shareVarFromZone(medium.DepthNvar)=0

                call TecplotVarAdd('LogDepth',medium.Nvar,medium.LogDepthNvar,medium.VarList)
                TBinary_vartype(medium.LogDepthNvar)=1  ! single precision
                TBinary_passiveVarList(medium.LogDepthNvar)=0
                TBinary_valueLocation(medium.LogDepthNvar)=1 ! node-centred
                TBinary_shareVarFromZone(medium.LogDepthNvar)=0
	            do i=1,medium.NN
		            if(.not. yVertical) then
		                medium.Depth(i)=medium.Head(i)-medium.Z(i)
                    else
		                medium.Depth(i)=medium.Head(i)-medium.Y(i)
                    end if
		            medium.Depth(i)=max(0.0,medium.Depth(i))
		            if(.not. yVertical) then
		                medium.LogDepth(i)=max(REAL(DepthMin,KIND=dr),medium.Head(i)-REAL(medium.Z(i)))
                    else
		                medium.LogDepth(i)=max(REAL(DepthMin,KIND=dr),medium.Head(i)-REAL(medium.Y(i)))
                    end if
		            medium.LogDepth(i)=log10(medium.LogDepth(i))
	            end do
            end if
        end if

    end subroutine HeadInit
    !------------------------------------------------------------------------
    subroutine HeadRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i

        filename=trim(LocalPrefix)//'o.head_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.HeadFileN)
        if(medium.HeadFileN) then

	        call read_var8(medium.NN,filename,medium.Head)
            
            if(medium.Name == 'pm') then
                do i=1,medium.NN
		            if(.not. yVertical) then
                        medium.P(i)=medium.Head(i)-medium.Z(i)
                    else
                        medium.P(i)=medium.Head(i)-medium.Y(i)
                    end if
                end do
            end if


	        if(medium.name == 'olf' .or. medium.name == 'chan') then
	            do i=1,medium.NN
                    if(.not. yVertical) then
		                medium.Depth(i)=medium.Head(i)-medium.Z(i)
                    else
		                medium.Depth(i)=medium.Head(i)-medium.Y(i)
                    end if
		            medium.Depth(i)=max(0.0,medium.Depth(i))
		            if(.not. yVertical) then
		                medium.LogDepth(i)=max(REAL(DepthMin,KIND=dr),medium.Head(i)-REAL(medium.Z(i)))
                    else
		                medium.LogDepth(i)=max(REAL(DepthMin,KIND=dr),medium.Head(i)-REAL(medium.Y(i)))
                    end if
		            medium.LogDepth(i)=log10(medium.LogDepth(i))
	            end do
	        end if

        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.HeadNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PNvar,medium.IDNum)=1
	            if(medium.name == 'olf' .or. medium.name == 'chan') then
                    ShareVarFromZone_M(medium.DepthNvar,medium.IDNum)=1
                    ShareVarFromZone_M(medium.LogDepthNvar,medium.IDNum)=1
                end if
            else
	            write(TMPStr,NFSFormat) medium.HeadNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
	            if(medium.name == 'olf' .or. medium.name == 'chan') then
	                write(TMPStr,NFSFormat) medium.DepthNvar
	                dlist=trim(dlist)//','//trim(TMPStr)
	                write(TMPStr,NFSFormat) medium.LogDepthNvar
	                dlist=trim(dlist)//','//trim(TMPStr)
                end if
            end if
        end if

    end subroutine HeadRead
    !------------------------------------------------------------------------
    subroutine DeltaZInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.DeltaZ_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.DeltaZFile)
    
        ! Z has already been read, just set the file exist switch here

    end subroutine DeltaZInit
    !------------------------------------------------------------------------
    subroutine DeltaZRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.DeltaZ_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.DeltaZFileN)
        if(medium.DeltaZFileN) then

	        call read_var8(medium.NN,filename,medium.Z)

        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ZNvar,medium.IDNum)=1
            else
	            write(TMPStr,NFSFormat) medium.ZNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

    end subroutine DeltaZRead
    !------------------------------------------------------------------------
    subroutine SatInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.sat_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.SatFile)
        if(medium.SatFile) then
            allocate(medium.Sat(medium.NN),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate Sat')
	            stop
            end if
            medium.Sat(:) = 0 
            call init_var4(medium.NN,filename,medium.Sat,'Sat',medium.SatNvar,medium.Nvar,medium.VarList)
        end if

    end subroutine SatInit
    !------------------------------------------------------------------------
    subroutine IceSatInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ice_sat_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.IceSatFile)
        if(medium.IceSatFile) then
            allocate(medium.IceSat(medium.NN),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate Sat')
	            stop
            end if
            medium.IceSat(:) = 0 
            call init_var4(medium.NN,filename,medium.IceSat,'IceSat',medium.IceSatNvar,medium.Nvar,medium.VarList)
        end if

    end subroutine IceSatInit
    !------------------------------------------------------------------------
    subroutine SatRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.sat_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.SatFileN)
        if(medium.SatFileN) then
	        call read_var4(medium.NN,filename,medium.Sat)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.SatNvar,medium.IDNum)=1
            else
	            write(TMPStr,NFSFormat) medium.SatNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

    end subroutine SatRead
    !------------------------------------------------------------------------
    subroutine IceSatRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ice_sat_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.IceSatFileN)
        if(medium.IceSatFileN) then
	        call read_var4(medium.NN,filename,medium.IceSat)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.IceSatNvar,medium.IDNum)=1
            else
	            write(TMPStr,NFSFormat) medium.IceSatNvar
	            dlist=trim(dlist)//','//TMPStr
            end if
        end if

    end subroutine IceSatRead
    !------------------------------------------------------------------------
    subroutine VelInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.v_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.VelFile)
        if(medium.VelFile) then

            allocate(medium.Vx(medium.NE),medium.Vy(medium.NE),medium.Vz(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate arrays Vx, Vy, Vz')
	            stop
            end if
            medium.Vx(:) = 0 
            medium.Vy(:) = 0 
            medium.Vz(:) = 0 

            call init_vec4(medium.NE,filename, medium.Vx, 'Vx', medium.VxNvar, &
						             medium.Vy, 'Vy', medium.VyNvar, &
						             medium.Vz, 'Vz', medium.VzNvar,medium.Nvar, medium.VarList)

            ! element (i.e. tecplot cell-centred) variable 
            write(temp_line,'(3(a,i7),a)') ',',medium.VxNvar,',',medium.VyNvar,',',medium.VzNvar
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine VelInit
    !------------------------------------------------------------------------
    subroutine VelRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.v_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.VelFileN)
        if(medium.VelFileN) then
            call read_vec4(medium.NE,filename,medium.Vx, medium.Vy, medium.Vz)
    	
        else   ! connect velocities
            if(BinaryMode) then
                ShareVarFromZone_M(medium.VxNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.VyNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.VzNvar,medium.IDNum)=1
            else
                write(TMPstr,NFSFormat) medium.VxNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.VyNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.VzNvar
                dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.VxNvar,',',medium.VyNvar,',',medium.VzNvar
        zone_line=trim(zone_line)//temp_line

    end subroutine VelRead
    !------------------------------------------------------------------------
    subroutine ETInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ETEvap_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ETFile)
        if(medium.ETFile) then
	        allocate(medium.Evap(medium.NN),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array Evap')
		        stop
	        end if
	        medium.Evap(:) = 0 
	        call init_var8(medium.NN,filename,medium.Evap,'Surface water evaporation',medium.EvapNvar,medium.Nvar,medium.VarList)

            filename=trim(LocalPrefix)//'o.ETPmEvap_'//trim(medium.name)//'.0001'
	        allocate(medium.PmEvap(medium.NN),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array PmEvap')
		        stop
	        end if
	        medium.PmEvap(:) = 0 
	        call init_var8(medium.NN,filename,medium.PmEvap,'Subsurface evaporation',medium.PmEvapNvar,medium.Nvar,medium.VarList)

            filename=trim(LocalPrefix)//'o.ETPmTranspire_'//trim(medium.name)//'.0001'
	        allocate(medium.PmTranspire(medium.NN),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array PmTranspire')
		        stop
	        end if
	        medium.PmTranspire(:) = 0 
	        call init_var8(medium.NN,filename,medium.PmTranspire,'Subsurface transpiration',medium.PmTranspireNvar,medium.Nvar,medium.VarList)

            filename=trim(LocalPrefix)//'o.ETTotal_'//trim(medium.name)//'.0001'
	        allocate(medium.ETTotal(medium.NN),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array ETTotal')
		        stop
	        end if
	        medium.ETTotal(:) = 0 
	        call init_var8(medium.NN,filename,medium.ETTotal,'Total ET',medium.ETTotalNvar,medium.Nvar,medium.VarList)

        end if

    end subroutine ETInit
    !------------------------------------------------------------------------
    subroutine ConcInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        integer :: i	

        
        filename=trim(LocalPrefix)//'o.conc_'//trim(medium.name)//'.'//trim(spname(1))//'.0001'
        inquire(file=filename,exist=medium.ConcFile)
        if(medium.ConcFile) then
	        allocate(medium.Conc(medium.NN,nspeciesmob),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array conc')
		        stop
	        end if
	        medium.Conc(:,:) = 0 
    !	    allocate(medium.ConcNvar(nspeciesmob),stat=status)
    !	    if(status /= 0) then
    !		    call Msg(' Failed to allocate array concnvar')
    !		    stop
    !	    end if
	        medium.ConcNvar(:) = 0 
	        do i=1,nspeciesmob
		        
		        filename=trim(LocalPrefix)//'o.conc_'//trim(medium.name)//'.'//trim(spname(i))//'.0001'
		        call init_var8(medium.NN,filename,medium.Conc(:,i),trim(spname(i)),medium.ConcNvar(i),medium.Nvar,medium.VarList)
	        end do

        end if
    end subroutine ConcInit
    !------------------------------------------------------------------------
    subroutine ExchFluxInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
	
        filename=trim(LocalPrefix)//'o.ExchFlux_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ExchFluxFile)
        if(medium.ExchFluxFile) then
	        allocate(medium.ExchFlux(medium.NN),stat=status)
	        if(status /= 0) then
		        call Msg(' Failed to allocate array ExchFlux')
		        stop
	        end if
	        medium.ExchFlux(:) = 0 
	        call init_var4(medium.NN,filename,medium.ExchFlux,'Exchange flux',medium.ExchFluxNvar,medium.Nvar,medium.VarList)
        end if
    end subroutine ExchFluxInit
    !------------------------------------------------------------------------
    subroutine ExchSolAdvInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i	

        
        filename=trim(LocalPrefix)//'o.ExchSolAdv_'//trim(medium.name)//'.'//trim(spname(1))//'.0001'
        inquire(file=filename,exist=medium.ExchSolAdvFile)
        if(medium.ExchSolAdvFile) then
	        allocate(medium.ExchSolAdv(medium.NN,nspeciesmob),medium.ExchSolAdvNvar(nspeciesmob),stat=status)
	        if(status /= 0) then
		        call Msg('Failed to allocate exch medium.ExchSolAdv')
		        stop
	        end if
	        medium.ExchSolAdv(:,:) = 0 
	        medium.ExchSolAdvNvar(:) = 0 
	        do i=1,nspeciesmob
		        
		        filename=trim(LocalPrefix)//'o.ExchSolAdv_'//trim(medium.name)//'.'//trim(spname(i))//'.0001'
		        call init_var4(medium.NN,filename,medium.ExchSolAdv(:,i),trim(spname(i))//' exchange',medium.ExchSolAdvNvar(i),medium.Nvar,medium.VarList)
	        end do

        end if
    end subroutine ExchSolAdvInit
    !------------------------------------------------------------------------
    subroutine ExchSolDispInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i	

        
        filename=trim(LocalPrefix)//'o.ExchSolDisp_'//trim(medium.name)//'.'//trim(spname(1))//'.0001'
        inquire(file=filename,exist=medium.ExchSolDispFile)
        if(medium.ExchSolDispFile) then
	        allocate(medium.ExchSolDisp(medium.NN,nspeciesmob),medium.ExchSolDispNvar(nspeciesmob),stat=status)
	        if(status /= 0) then
		        call Msg('Failed to allocate exch medium.ExchSolDisp')
		        stop
	        end if
	        medium.ExchSolDisp(:,:) = 0 
	        medium.ExchSolDispNvar(:) = 0 
	        do i=1,nspeciesmob
		        
		        filename=trim(LocalPrefix)//'o.ExchSolDisp_'//trim(medium.name)//'.'//trim(spname(i))//'.0001'
		        call init_var4(medium.NN,filename,medium.ExchSolDisp(:,i),trim(spname(i))//' disp. exchange',medium.ExchSolDispNvar(i),medium.Nvar,medium.VarList)
	        end do

        end if
    end subroutine ExchSolDispInit
    !------------------------------------------------------------------------
    subroutine ETRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ETEvap_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ETFilen)
        if(medium.ETFilen) then
	        call read_var8(medium.NN,filename,medium.Evap)

            filename=trim(LocalPrefix)//'o.ETPmEvap_'//trim(medium.name)//'.'//nfs
	        call read_var8(medium.NN,filename,medium.PmEvap)

            filename=trim(LocalPrefix)//'o.ETPmTranspire_'//trim(medium.name)//'.'//nfs
	        call read_var8(medium.NN,filename,medium.PmTranspire)

            filename=trim(LocalPrefix)//'o.ETTotal_'//trim(medium.name)//'.'//nfs
	        call read_var8(medium.NN,filename,medium.ETTotal)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.EvapNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PmEvapNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PmTranspireNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.ETTotalNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.EvapNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
	            write(TMPstr,NFSFormat) medium.PmEvapNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
	            write(TMPstr,NFSFormat) medium.PmTranspireNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
	            write(TMPstr,NFSFormat) medium.ETTotalNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

    end subroutine ETRead
    !------------------------------------------------------------------------
    subroutine ExchFluxRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ExchFlux_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ExchFluxFileN)
        if(medium.ExchFluxFileN) then
	        call read_var4(medium.NN,filename,medium.ExchFlux)

        else
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ExchFluxNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ExchFluxNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

    end subroutine ExchFluxRead
    !------------------------------------------------------------------------
    subroutine ConcRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i

        do i=1,nspeciesmob
	        
            filename=trim(LocalPrefix)//'o.conc_'//trim(medium.name)//'.'//trim(spname(i))//'.'//nfs
	        inquire(file=filename,exist=medium.ConcFileN)
	        if(medium.ConcFileN) then
		        call read_var8(medium.NN,filename,medium.Conc(:,i))

	        else ! connect mobile concentrations
                if(BinaryMode) then
                    ShareVarFromZone_M(medium.ConcNvar(i),medium.IDNum)=1
                else
		            write(TMPstr,NFSFormat) medium.ConcNvar(i)
		            dlist=trim(dlist)//','//trim(TMPStr)
                end if
	        end if
        end do
    end subroutine ConcRead
    !------------------------------------------------------------------------
    subroutine ExchSolAdvRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i

        do i=1,nspeciesmob
	        
            filename=trim(LocalPrefix)//'o.ExchSolAdv_'//trim(medium.name)//'.'//trim(spname(1))//'.'//nfs
	        inquire(file=filename,exist=medium.ExchSolAdvFileN)
	        if(medium.ExchSolAdvFileN) then
		        call read_var4(medium.NN,filename,medium.ExchSolAdv(:,i))

	        else
                if(BinaryMode) then
                    ShareVarFromZone_M(medium.ExchSolAdvNvar(i),medium.IDNum)=1
                else
		            write(TMPstr,NFSFormat) medium.ExchSolAdvNvar(i)
		            dlist=trim(dlist)//','//trim(TMPStr)
                end if
	        end if
        end do
    end subroutine ExchSolAdvRead
    !------------------------------------------------------------------------
    subroutine ExchSolDispRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium
        integer :: i

        do i=1,nspeciesmob
	        
            filename=trim(LocalPrefix)//'o.ExchSolDisp_'//trim(medium.name)//'.'//trim(spname(1))//'.'//nfs
	        inquire(file=filename,exist=medium.ExchSolDispFileN)
	        if(medium.ExchSolDispFileN) then
		        call read_var4(medium.NN,filename,medium.ExchSolDisp(:,i))

	        else
                if(BinaryMode) then
                    ShareVarFromZone_M(medium.ExchSolDispNvar(i),medium.IDNum)=1
                else
		            write(TMPstr,NFSFormat) medium.ExchSolDispNvar(i)
		            dlist=trim(dlist)//','//trim(TMPStr)
                end if
	        end if
        end do
    end subroutine ExchSolDispRead
    !------------------------------------------------------------------------
    subroutine ElemKInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemK_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ElemKFile)
        if(medium.ElemKFile) then

            allocate(medium.ElemKx(medium.Ne),medium.ElemKy(medium.Ne),medium.ElemKz(medium.Ne),stat=status)
            if(status /= 0) then
                call Msg(' failed to allocate arrays elemental k')
                stop
            end if
            medium.ElemKx(:) = 0 
            medium.ElemKy(:) = 0 
            medium.ElemKz(:) = 0 

            call init_vec8(medium.Ne,filename, medium.ElemKx, 'Kxx', medium.ElemKxNvar, &
							            medium.ElemKy, 'Kyy', medium.ElemKyNvar, &
							            medium.ElemKz, 'Kzz', medium.ElemKzNvar,medium.Nvar,medium.VarList)


            ! element (i.e. tecplot cell-centred) variable 
            write(temp_line,'(3(a,i7),a)') ',',medium.ElemKxNvar,',',medium.ElemKyNvar,',',medium.ElemKzNvar
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine ElemKInit
    !------------------------------------------------------------------------
    subroutine ElemKRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemK_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ElemKFileN)
        if(medium.ElemKFileN) then
            call read_vec8(medium.NE,filename,medium.ElemKx, medium.ElemKy, medium.ElemKz)
    	
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ElemKxNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.ElemKyNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.ElemKzNvar,medium.IDNum)=1
            else
                write(TMPstr,NFSFormat) medium.ElemKxNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.ElemKyNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.ElemKzNvar
                dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.ElemKxNvar,',',medium.ElemKyNvar,',',medium.ElemKzNvar
        zone_line=trim(zone_line)//temp_line

    end subroutine ElemKRead
    !------------------------------------------------------------------------
    subroutine TVKInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.TVK_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.TVKFile)
        if(medium.TVKFile) then

            allocate(medium.TVKx(medium.Ne),medium.TVKy(medium.Ne),medium.TVKz(medium.Ne),stat=status)
            if(status /= 0) then
                call Msg(' failed to allocate arrays elemental k')
                stop
            end if
            medium.TVKx(:) = 0 
            medium.TVKy(:) = 0 
            medium.TVKz(:) = 0 

            call init_vec4(medium.Ne,filename, medium.TVKx, 'Time varying Kxx', medium.TVKxNvar, &
							            medium.TVKy, 'Time varying Kyy', medium.TVKyNvar, &
							            medium.TVKz, 'Time varying Kzz', medium.TVKzNvar,medium.Nvar,medium.VarList)


            ! element (i.e. tecplot cell-centred) variable 
            write(temp_line,'(3(a,i7),a)') ',',medium.TVKxNvar,',',medium.TVKyNvar,',',medium.TVKzNvar
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine TVKInit
    !------------------------------------------------------------------------
    subroutine TVKRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.TVK_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.TVKFileN)
        if(medium.TVKFileN) then
            call read_vec4(medium.NE,filename,medium.TVKx, medium.TVKy, medium.TVKz)
    	
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.TVKxNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.TVKyNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.TVKzNvar,medium.IDNum)=1
            else
                write(TMPstr,NFSFormat) medium.TVKxNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.TVKyNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.TVKzNvar
                dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.TVKxNvar,',',medium.TVKyNvar,',',medium.TVKzNvar
        zone_line=trim(zone_line)//temp_line

    end subroutine TVKRead
    !------------------------------------------------------------------------
    subroutine ElemPorInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemPor_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ElemPorFile)
        if(medium.ElemPorFile) then
            allocate(medium.ElemPor(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate ElemPor')
	            stop
            end if
            medium.ElemPor(:) = 0 
            call init_var4(medium.NE,filename,medium.ElemPor,'Porosity',medium.ElemPorNvar,medium.Nvar,medium.VarList)

            write(temp_line,'(3(a,i7),a)') ',',medium.ElemPorNvar
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine ElemPorInit
    !------------------------------------------------------------------------
    subroutine ElemPorRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemPor_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ElemPorFileN)
        if(medium.ElemPorFileN) then
	        call read_var4(medium.NE,filename,medium.ElemPor)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ElemPorNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ElemPorNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.ElemPorNvar
        zone_line=trim(zone_line)//temp_line


    end subroutine ElemPorRead
    !------------------------------------------------------------------------
    subroutine ElemStorInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemStor_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ElemStorFile)
        if(medium.ElemStorFile) then
            allocate(medium.ElemStor(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate ElemStor')
	            stop
            end if
            medium.ElemStor(:) = 0 
            call init_var8(medium.NE,filename,medium.ElemStor,'SpStor',medium.ElemStorNvar,medium.Nvar,medium.VarList)

            write(temp_line,'(3(a,i7),a)') ',',medium.ElemStorNvar
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine ElemStorInit
    !------------------------------------------------------------------------
    subroutine ElemStorRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemStor_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ElemStorFileN)
        if(medium.ElemStorFileN) then
	        call read_var8(medium.NE,filename,medium.ElemStor)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ElemStorNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ElemStorNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.ElemStorNvar
        zone_line=trim(zone_line)//temp_line


    end subroutine ElemStorRead
    !------------------------------------------------------------------------
    subroutine ElemTortInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemTort_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ElemTortFile)
        if(medium.ElemTortFile) then
            allocate(medium.ElemTort(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate ElemTort')
	            stop
            end if
            medium.ElemTort(:) = 0 
            call init_var8(medium.NE,filename,medium.ElemTort,'Tortuosity',medium.ElemTortNvar,medium.Nvar,medium.VarList)

            write(temp_line,'(3(a,i7),a)') ',',medium.ElemTortNvar
             
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine ElemTortInit
    !------------------------------------------------------------------------
    subroutine ElemTortRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemTort_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ElemTortFileN)
        if(medium.ElemTortFileN) then
	        call read_var8(medium.NE,filename,medium.ElemTort)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ElemTortNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ElemTortNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.ElemTortNvar
         
        zone_line=trim(zone_line)//temp_line

    end subroutine ElemTortRead
    !------------------------------------------------------------------------
    subroutine ElemIbedFractionInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemIbedFraction_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.ElemIbedFractionFile)
        if(medium.ElemIbedFractionFile) then
            allocate(medium.ElemIbedFraction(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate ElemIbedFraction')
	            stop
            end if
            medium.ElemIbedFraction(:) = 0 
            call init_var8(medium.NE,filename,medium.ElemIbedFraction,'ElemIbedFraction',medium.ElemIbedFractionNvar,medium.Nvar,medium.VarList)
 
            write(temp_line,'(3(a,i7),a)') ',',medium.ElemIbedFractionNvar
             
            zone_line=trim(zone_line)//temp_line

       end if

    end subroutine ElemIbedFractionInit
    !------------------------------------------------------------------------
    subroutine ElemIbedFractionRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.ElemIbedFraction_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.ElemIbedFractionFileN)
        if(medium.ElemIbedFractionFileN) then
	        call read_var8(medium.NE,filename,medium.ElemIbedFraction)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.ElemTortNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.ElemIbedFractionNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if
    
        write(temp_line,'(3(a,i7),a)') ',',medium.ElemIbedFractionNvar
         
        zone_line=trim(zone_line)//temp_line


    end subroutine ElemIbedFractionRead
    !------------------------------------------------------------------------
    subroutine PermafrostInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.Permafrost_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.PermafrostFile)
        if(medium.PermafrostFile) then
            allocate(medium.Permafrost(medium.NE),stat=status)
            if(status /= 0) then
	            call Msg(' Failed to allocate Permafrost')
	            stop
            end if
            medium.Permafrost(:) = 0 
            call init_var4(medium.NE,filename,medium.Permafrost,'Porosity',medium.PermafrostNvar,medium.Nvar,medium.VarList)

            write(temp_line,'(3(a,i7),a)') ',',medium.PermafrostNvar
             
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine PermafrostInit
    !------------------------------------------------------------------------
    subroutine PermafrostRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.Permafrost_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.PermafrostFileN)
        if(medium.PermafrostFileN) then
	        call read_var4(medium.NE,filename,medium.Permafrost)
        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.PermafrostNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.PermafrostNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.PermafrostNvar
         
        zone_line=trim(zone_line)//temp_line


    end subroutine PermafrostRead

    !------------------------------------------------------------------------
    subroutine PecletInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.Peclet_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.PecletFile)
        if(medium.PecletFile) then

            allocate(medium.Pecletx(medium.Ne),medium.Peclety(medium.Ne),medium.Pecletz(medium.Ne),stat=status)
            if(status /= 0) then
                call Msg(' failed to allocate arrays elemental k')
                stop
            end if
            medium.Pecletx(:) = 0 
            medium.Peclety(:) = 0 
            medium.Pecletz(:) = 0 

            call init_vec4(medium.Ne,filename, medium.Pecletx, 'Peclet x', medium.PecletxNvar, &
							            medium.Peclety, 'Peclet y', medium.PecletyNvar, &
							            medium.Pecletz, 'Peclet z', medium.PecletzNvar,medium.Nvar,medium.VarList)


            ! element (i.e. tecplot cell-centred) variable 
            write(temp_line,'(3(a,i7),a)') ',',medium.PecletxNvar,',',medium.PecletyNvar,',',medium.PecletzNvar
             
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine PecletInit
    !------------------------------------------------------------------------
    subroutine PecletRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.Peclet_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.PecletFileN)
        if(medium.PecletFileN) then
            call read_vec4(medium.NE,filename,medium.Pecletx, medium.Peclety, medium.Pecletz)
    	
        else   ! connect velocities
            if(BinaryMode) then
                ShareVarFromZone_M(medium.PecletxNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PecletyNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.PecletzNvar,medium.IDNum)=1
            else
                write(TMPstr,NFSFormat) medium.PecletxNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.PecletyNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.PecletzNvar
                dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.PecletxNvar,',',medium.PecletyNvar,',',medium.PecletzNvar
         
        zone_line=trim(zone_line)//temp_line

    end subroutine PecletRead
    !------------------------------------------------------------------------
    subroutine DiffPecletInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.DiffPeclet_'//trim(medium.name)//'.0001'
        inquire(file=filename,exist=medium.DiffPecletFile)
        if(medium.DiffPecletFile) then

            allocate(medium.DiffPecletx(medium.Ne),medium.DiffPeclety(medium.Ne),medium.DiffPecletz(medium.Ne),stat=status)
            if(status /= 0) then
                call Msg(' failed to allocate arrays elemental k')
                stop
            end if
            medium.DiffPecletx(:) = 0 
            medium.DiffPeclety(:) = 0 
            medium.DiffPecletz(:) = 0 

            call init_vec4(medium.Ne,filename, medium.DiffPecletx, 'DiffPeclet x', medium.DiffPecletxNvar, &
							            medium.DiffPeclety, 'DiffPeclet y', medium.DiffPecletyNvar, &
							            medium.DiffPecletz, 'DiffPeclet z', medium.DiffPecletzNvar,medium.Nvar,medium.VarList)


            ! element (i.e. tecplot cell-centred) variable 
            write(temp_line,'(3(a,i7),a)') ',',medium.DiffPecletxNvar,',',medium.DiffPecletyNvar,',',medium.DiffPecletzNvar
             
            zone_line=trim(zone_line)//temp_line

        end if

    end subroutine DiffPecletInit
    !------------------------------------------------------------------------
    subroutine DiffPecletRead(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        filename=trim(LocalPrefix)//'o.DiffPeclet_'//trim(medium.name)//'.'//nfs
        inquire(file=filename,exist=medium.DiffPecletFileN)
        if(medium.DiffPecletFileN) then
            call read_vec4(medium.NE,filename,medium.DiffPecletx, medium.DiffPeclety, medium.DiffPecletz)
    	
        else   ! connect velocities
            if(BinaryMode) then
                ShareVarFromZone_M(medium.DiffPecletxNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.DiffPecletyNvar,medium.IDNum)=1
                ShareVarFromZone_M(medium.DiffPecletzNvar,medium.IDNum)=1
            else
                write(TMPstr,NFSFormat) medium.DiffPecletxNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.DiffPecletyNvar
                dlist=trim(dlist)//','//trim(TMPStr)
                write(TMPstr,NFSFormat) medium.DiffPecletzNvar
                dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

        write(temp_line,'(3(a,i7),a)') ',',medium.DiffPecletxNvar,',',medium.DiffPecletyNvar,',',medium.DiffPecletzNvar
         
        zone_line=trim(zone_line)//temp_line

    end subroutine DiffPecletRead
    !------------------------------------------------------------------------
    !Calculate_Depth_to_Groundwater_Table
    subroutine Depth2GWTInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium

        real(dr), allocatable :: p_head(:) 
        real(dr), allocatable :: elev_GWT(:)

        integer :: i, j, m, nn2d 

        real(dr) :: p1, p2, z1, z2 

        if(medium.HeadFile) then
            allocate(medium.Depth2GWT(medium.nn),stat=status)
            if(status /= 0) then
                call Msg('Failed to allocate array Depth2GWTInit')
                stop
            end if
            call TecplotVarAdd('Depth2GWT',medium.Nvar,medium.Depth2GWTNvar,medium.VarList)
            TBinary_vartype(medium.Nvar)=1 ! single precision
            TBinary_passiveVarList(medium.Nvar)=0
            TBinary_valueLocation(medium.Nvar)=1 ! node-centred
            TBinary_shareVarFromZone(medium.Nvar)=0

            allocate(p_head(medium.nn),elev_GWT(medium.nn),stat=status)
            if(status /= 0) then
                call Msg('Failed to allocate array p_head, elev_GWT')
                stop
            end if

            nn2d = medium.nn/nz
      
            p_head = 0.0

            do i=1,medium.NN
                p_head(i) = medium.Head(i) - medium.Z(i)
            end do

            do i=medium.NN-nn2d+1, medium.NN
                !calculate the elevation at presure head = 0.0
                if(p_head(i) >= 0.0) then
                    elev_GWT(i) = medium.Z(i)
                else
                    vertical_loop:do m=1, nz-1
                        if(p_head(i-m*nn2d) >= 0.0) then
                            p1 = p_head(i-m*nn2d+nn2d)
                            p2 = p_head(i-m*nn2d)
                            z1 = medium.Z(i-m*nn2d+nn2d)
                            z2 = medium.Z(i-m*nn2d)
                            elev_GWT(i) = z2 - p2/(p2-p1)*(z2-z1)
                            exit vertical_loop
                        else if(m == nz-1) then !this IF statement is for the case that even bottom is not saturated
                            elev_GWT(i) = medium.Z(i-m*nn2d)
                        end if
                    end do vertical_loop
                end if
            
                do j=0, nz-1 !compute the depth to water table
                    medium.Depth2GWT(i-j*nn2d) = medium.Z(i) - elev_GWT(i)
                end do
            end do

            deallocate(p_head)
            deallocate(elev_GWT)
        end if

    end subroutine Depth2GWTInit
    !------------------------------------------------------------------------
    subroutine Depth2GWTRead(medium)
         
        implicit none
    
        type(HGSTecplotdomain) medium
    
        real(dr), allocatable :: p_head(:) 
        real(dr), allocatable :: elev_GWT(:)

        integer :: i, j, m, nn2d 

        real(dr) :: p1, p2, z1, z2 

        if(medium.HeadFileN) then

            nn2d = medium.nn/nz
      
            allocate(p_head(medium.nn),elev_GWT(medium.nn),stat=status)
            if(status /= 0) then
                call Msg('Failed to allocate array p_head, elev_GWT')
                stop
            end if

            p_head = 0.0

            do i=1,medium.NN
                p_head(i) = medium.Head(i) - medium.Z(i)
            end do

            do i=medium.NN-nn2d+1, medium.NN
                !calculate the elevation at presure head = 0.0
                if(p_head(i) >= 0.0) then
                    elev_GWT(i) = medium.Z(i)
                else
                    vertical_loop:do m=1, nz-1
                        if(p_head(i-m*nn2d) >= 0.0) then
                            p1 = p_head(i-m*nn2d+nn2d)
                            p2 = p_head(i-m*nn2d)
                            z1 = medium.Z(i-m*nn2d+nn2d)
                            z2 = medium.Z(i-m*nn2d)
                            elev_GWT(i) = z2 - p2/(p2-p1)*(z2-z1)
                            exit vertical_loop
                        else if(m == nz-1) then !this IF statement is for the case that even bottom is not saturated
                            elev_GWT(i) = medium.Z(i-m*nn2d)
                        end if
                    end do vertical_loop
                end if
            
                do j=0, nz-1 !compute the depth to water table
                    medium.Depth2GWT(i-j*nn2d) = medium.Z(i) - elev_GWT(i)
                end do
            end do

            deallocate(p_head)
            deallocate(elev_GWT)

        else   ! connect 
            if(BinaryMode) then
                ShareVarFromZone_M(medium.Depth2GWTNvar,medium.IDNum)=1
            else
	            write(TMPstr,NFSFormat) medium.Depth2GWTNvar
	            dlist=trim(dlist)//','//trim(TMPStr)
            end if
        end if

    end subroutine Depth2GWTRead
    !------------------------------------------------------------------------
    !Feflow Slice numbers
    subroutine FsliceInit(medium)
         
        implicit none
        type(HGSTecplotdomain) medium


        integer :: i, j, i1, nn2d 

        allocate(medium.Fslice(medium.nn),stat=status)
        if(status /= 0) then
            call Msg('Failed to allocate array Fslice')
            stop
        end if
        call TecplotVarAdd('Fslice',medium.Nvar,medium.FsliceNvar,medium.VarList)
        TBinary_vartype(medium.Nvar)=3 ! integer
        TBinary_passiveVarList(medium.Nvar)=0
        TBinary_valueLocation(medium.Nvar)=1 ! node-centred
        TBinary_shareVarFromZone(medium.Nvar)=0

        nn2d = medium.nn/nz
        ! Calculate Feflow slice number
        i1=0
        do i=nz,1,-1
            do j=1,nn2d
                i1=i1+1
                medium.FSlice(i1)=i
            end do
        end do
            

    end subroutine FsliceInit
    !------------------------------------------------------------------------
    subroutine WritePlotControlInit
        implicit none
	
	    write(PlotControlUnit,'(a)') ' '//tecplot_mode_str
	    write(PlotControlUnit,'(a)') '! '//gms_mode_str
	    write(PlotControlUnit,'(a)') '! '//double_xy_str
	    write(PlotControlUnit,'(a)') 'end'
    
    end subroutine WritePlotControlInit
    !------------------------------------------------------------------------
    subroutine WritePlotControl(medium)
        implicit none
        type(HGSTecplotdomain) medium
    
        ! this subroutine is designed to read what hgs produces and there are inconsistencies
    
        if(.not. medium.Exists) return

        write(PlotControlUnit,'(a)') medium.Plot_str 

    !if(icurprop.eq.nprop_head) then
        write(PlotControlUnit,'(a)') '! '//medium.HeadOutput_str 

    !elseif(icurprop.eq.nprop_saturation) then
        if(medium.name == 'pm' .or. medium.name == 'dual' .or. medium.name == 'frac') then
            write(PlotControlUnit,'(a)') '! '//medium.SatOutput_str 
        end if
        if(medium.name == 'pm') then
            write(PlotControlUnit,'(a)') '! '//medium.IceSatOutput_str
        end if

        write(PlotControlUnit,'(a)') '! '//medium.VelOutput_str 

    !elseif(icurprop.eq.concentration) then
    !elseif(icurprop.eq.chemical_species) then ! has .conc_ filename so should be handled by same routine
        write(PlotControlUnit,'(a)') '! '//medium.ConcOutput_str 
        write(PlotControlUnit,'(a)') '! '//medium.IConcOutput_str 

        ! written by grok for element-variable properties
        if(medium.name == 'pm') then
            write(PlotControlUnit,'(a)') '! '//medium.PecletOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.DiffPecletOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemTortOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemKOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.TVKOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemPorOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemStorOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemTortOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.ElemIbedFractionOutput_str 
        end if
        if(medium.name == 'dual') then
            write(PlotControlUnit,'(a)') '! '//medium.ElemKOutput_str 
            write(PlotControlUnit,'(a)') '! '//medium.TVKOutput_str 
        end if

        if(medium.name == 'frac') then
            write(PlotControlUnit,'(a)') '! '//medium.ApertureOutput_str 
        end if

        if(medium.name == 'pm') then
    !elseif(icurprop.eq.permafrost)then
            write(PlotControlUnit,'(a)') '! '//medium.PermafrostOutput_str 
    !elseif(icurprop.eq.soilfrost)then
            write(PlotControlUnit,'(a)') '! '//medium.SoilfrostOutput_str 
    !elseif(icurprop.eq.nodal_compaction) then
            write(PlotControlUnit,'(a)') '! '//medium.CompactOutput_str 
    !elseif(icurprop.eq.head) then
            write(PlotControlUnit,'(a)') '! '//medium.DeltaZOutput_str 
        end if
    
    
 
        if(medium.name /= 'pm') then
    ! written for olf and frac only and if(icurprop.eq.nprop_head) then
            write(PlotControlUnit,'(a)') '! '//medium.ExchFluxOutput_str 
        end if

        if(medium.name /= 'pm') then
            write(PlotControlUnit,'(a)') '! '//medium.ExchSolOutput_str 
        end if
   
    !elseif(icurprop.eq.et_output)then
        if(medium.name == 'olf') then
            write(PlotControlUnit,'(a)') '! '//medium.ETOutput_str  
        end if
    
        if(medium.name == 'pm') then
            write(PlotControlUnit,'(a)') '! '//medium.Depth2GWTOutput_str  
        end if

        write(PlotControlUnit,'(a)') '! '//medium.DomainTruncate_str 
        write(PlotControlUnit,'(a,2e12.5)') '! ',medium.XminTrunc,medium.XmaxTrunc
        write(PlotControlUnit,'(a,2e12.5)') '! ',medium.YminTrunc,medium.YmaxTrunc
        write(PlotControlUnit,'(a,2e12.5)') '! ',medium.ZminTrunc,medium.ZmaxTrunc
    
        if(medium.name == 'pm') then
            inquire(file=trim(LocalPrefix)//'.observed_heads',exist=ObservedHeadsFileExists)
            filename=trim(LocalPrefix)//'o.head_'//trim(medium.name)//'.0001'
            inquire(file=filename,exist=medium.HeadFile)
            if(medium.HeadFile .and. ObservedHeadsFileExists) then
		        write(PlotControlUnit,'(a)') '! '//compare_heads_3d_domain_str
	            write(PlotControlUnit,'(a,2g15.8)') '! ',-1e20,1e20
	            write(PlotControlUnit,'(a,2g15.8)') '! ',-1e20,1e20
	            write(PlotControlUnit,'(a,2g15.8)') '! ',-1e20,1e20
	        end if
        end if


	    write(PlotControlUnit,'(a)') 'end'

        write(PlotControlUnit,'(a)') '!---------------------------------------------' 
    
 
    end subroutine WritePlotControl
    !------------------------------------------------------------------------
    subroutine ReadPlotControlInit
        implicit none
 	    do
		    read(PlotControlUnit,'(a60)',iostat=status) plot_instruction
		    if(status /= 0) exit

		    write(ieco,'(a)') ' '
		    write(ieco,'(a,a)') 'INSTRUCTION: ',plot_instruction

		    call LwrCse(plot_instruction)

		    if(plot_instruction .eq. tecplot_mode_str) then
			    tecplot_mode=.true.

		    else if(plot_instruction .eq. gms_mode_str) then
			    gms_mode=.true.

		    else if(plot_instruction .eq. double_xy_str) then
			    double_xy=.true.
		
		    else if(plot_instruction .eq. truncate_time_domain_str) then
			    truncate_time_domain=.true.
			    read(PlotControlUnit,*) tmin_trunc,tmax_trunc

		    else if(plot_instruction(1:3) .eq. 'end') then
			    exit
    
 	        else
			    call Msg('Plot control - Unrecognized line')
			    call Msg(plot_instruction)
			    call Msg(' ')
		    end if
	    end do

   
    end subroutine ReadPlotControlInit
    !------------------------------------------------------------------------
    subroutine ReadPlotControl(medium)
        implicit none
        type(HGSTecplotdomain) medium
    
        ! this subroutine is designed to read what hgs produces and there are inconsistencies
    
        if(.not. medium.Exists) return

	    medium.Plot=.false.

	    do
		    read(PlotControlUnit,'(a60)',iostat=status) plot_instruction
		    if(status /= 0) exit

		    write(ieco,'(a)') ' '
		    write(ieco,'(a,a)') 'INSTRUCTION: ',plot_instruction

		    call LwrCse(plot_instruction)

		    if(plot_instruction .eq. medium.Plot_str) then
			    medium.Plot=.true.

		    else if(plot_instruction .eq. medium.HeadOutput_str) then
			    medium.HeadOutput=.false.

		    else if(plot_instruction .eq. medium.SatOutput_str) then
			    medium.SatOutput=.false.

            else if(plot_instruction .eq. medium.IceSatOutput_str) then
                medium.IceSatOutput=.false.
        
		    else if(plot_instruction .eq. medium.VelOutput_str) then
			    medium.VelOutput=.false.

		    else if(plot_instruction .eq. medium.CompactOutput_str) then
			    medium.CompactOutput=.false.

		    else if(plot_instruction .eq. medium.ConcOutput_str) then
			    medium.ConcOutput=.false.

		    else if(plot_instruction .eq. medium.IConcOutput_str) then
			    medium.IConcOutput=.false.

		    else if(plot_instruction .eq. medium.PecletOutput_str) then
			    medium.PecletOutput=.false.

		    else if(plot_instruction .eq. medium.DiffPecletOutput_str) then
			    medium.DiffPecletOutput=.false.

		    else if(plot_instruction .eq. medium.ElemKOutput_str) then
			    medium.ElemKOutput=.false.

		    else if(plot_instruction .eq. medium.TVKOutput_str) then
			    medium.TVKOutput=.false.

		    else if(plot_instruction .eq. medium.ElemPorOutput_str) then
			    medium.ElemPorOutput=.false.

		    else if(plot_instruction .eq. medium.ElemStorOutput_str) then
			    medium.ElemStorOutput=.false.

		    else if(plot_instruction .eq. medium.ElemTortOutput_str) then
			    medium.ElemTortOutput=.false.

		    else if(plot_instruction .eq. medium.ElemIbedFractionOutput_str) then
			    medium.ElemIbedFractionOutput=.false.

		    else if(plot_instruction .eq. medium.ApertureOutput_str) then
			    medium.ApertureOutput=.false.

		    else if(plot_instruction .eq. medium.PermafrostOutput_str) then
			    medium.PermafrostOutput=.false.

		    else if(plot_instruction .eq. medium.SoilFrostOutput_str) then
			    medium.SoilFrostOutput=.false.

		    else if(plot_instruction .eq. medium.CompactOutput_str) then
			    medium.CompactOutput=.false.

		    else if(plot_instruction .eq. medium.DeltaZOutput_str) then
			    medium.DeltaZOutput=.false.

		    else if(plot_instruction .eq. medium.ExchFluxOutput_str) then
			    medium.ExchFluxOutput=.false.

		    else if(plot_instruction .eq. medium.ExchSolOutput_str) then
			    medium.ExchSolOutput=.false.

		    else if(plot_instruction .eq. medium.ETOutput_str) then
			    medium.ETOutput=.false.

		    else if(plot_instruction .eq. medium.Depth2GWTOutput_str) then
			    medium.Depth2GWTOutput=.false.

    !		else if(plot_instruction .eq. medium.VDarcyOutput_str) then
    !			medium.VDarcyOutput=.false.

		    else if(plot_instruction .eq. isolate_node_str) then
			    isolate_node=.true.
			    read(PlotControlUnit,*) iso_node
			    read(PlotControlUnit,*) n_extend

		    else if(plot_instruction .eq. medium.DomainTruncate_str) then
			     medium.DomainTruncate=.true.
			    read(PlotControlUnit,*) medium.XminTrunc,medium.XmaxTrunc
			    read(PlotControlUnit,*) medium.YminTrunc,medium.YmaxTrunc
			    read(PlotControlUnit,*) medium.ZminTrunc,medium.ZmaxTrunc

		    else if(medium.name=='pm' .and. (plot_instruction .eq. compare_heads_3d_domain_str)) then
			    compare_heads_3d_domain=.true.
			    read(PlotControlUnit,*) xmin_trunc_h,xmax_trunc_h 
			    read(PlotControlUnit,*) ymin_trunc_h,ymax_trunc_h 
			    read(PlotControlUnit,*) zmin_trunc_h,zmax_trunc_h 

		    else if(plot_instruction(1:3) .eq. 'end') then
			    exit

            else
			    call Msg('Plot control - Unrecognized line')
			    call Msg(plot_instruction)
			    call Msg(' ')
		    end if
	    end do

     end subroutine ReadPlotControl
    !!!!------------------------------------------------------------------------
    !!!subroutine read_obs_heads(medium)
    !!!    implicit none
    !!!
    !!!    type(HGSTecplotdomain) medium
    !!!
    !!!    integer :: i
    !!!
    !!!    call getunit(itmp)
    !!!    OPEN(itmp,FILE = trim(LocalPrefix)//'.observed_heads', &
    !!!        action = 'read', &
    !!!        form = 'formatted', &
    !!!        iostat = status)
    !!!    if(status /= 0) then
    !!!        call Msg( 'FILE ERROR: '//trim(LocalPrefix)//'.observed_heads'
    !!!        stop
    !!!    end if
	   !!!
    !!!    read(itmp,*) nobs_head
    !!!    allocate(xob(nobs_head),yob(nobs_head),zob(nobs_head),hob(nobs_head), &
    !!!            basisfunctions(nobs_head,8), obs_el(nobs_head) ,stat=status)
    !!!
    !!!    if(status /= 0) then
    !!!        call Msg( 'ALLOCATION ERROR:  in read_obs_heads'
    !!!                stop
    !!!    end if
    !!!
    !!!    do i=1,nobs_head
    !!!        read(itmp,*) xob(i),yob(i),zob(i),hob(i)
    !!!    end do
    !!!
    !!!    call freeunit(itmp)
	   !!!
    !!!    if(nln == 8) then   ! compute and store shape_functions and element #'s for each observation point using Fabien's routine
	   !!!     call obs_points_shape(medium)
    !!!    else
    !!!        ! do nothing here and later just use nearest node
    !!!    end if
    !!!
    !!!
    !!!end subroutine read_obs_heads
    !!!
    !!!!------------------------------------------------------------------------
    !!!subroutine compare_heads_output(medium)
	   !!! implicit none
    !!!
    !!!    type(HGSTecplotdomain) medium
    !!!
	   !!! integer :: i, j, nde
	   !!! real(dr) :: hmin, hmax
	   !!! real(dr) :: ybar, fbar, sstot, sserr, r2
	   !!!
	   !!! real(dr) :: nodal_function(8),basis_function(8)
	   !!! real(dr) :: head_interp
	   !!!
	   !!! real(dr) :: dfe_interp
	   !!!
	   !!! integer :: nobs_head_found
    !!!
	   !!! hmin=1.e20
	   !!! hmax=-1.e20
    !!!
	   !!! call getunit(itmp)
	   !!! OPEN(itmp,file = trim(LocalPrefix)//'o.compare_heads.dat',status = 'replace',form = 'formatted',iostat = status)
	   !!! if(status /= 0) then
		  !!!  call Msg('FILE ERROR: '//trim(LocalPrefix)//'o.compare_heads.dat')
		  !!!  stop
	   !!! end if
	   !!! write(itmp,'(a)') 'Title = "'//trim(LocalPrefix)//'"'
	   !!! write(itmp,'(a)') 'VARIABLES ="Observed head","Simulated head"'
	   !!! write(itmp,'(a)') 'zone t="data"' 
    !!!
    !!!    ! scatter plot
	   !!! call getunit(itmp2)
	   !!! OPEN(itmp2,file = trim(LocalPrefix)//'o.compare_heads_scatter.dat',status = 'replace',form = 'formatted',iostat = status)
	   !!! if(status /= 0) then
		  !!!  call Msg('FILE ERROR: '//trim(LocalPrefix)//'o.compare_heads.dat')
		  !!!  stop
	   !!! end if
	   !!! write(itmp2,'(a)') 'Title = " Observed vs simulated heads scatter plot"'
	   !!! write(itmp2,'(a)') 'VARIABLES = "X","Y","Z","Observed head","Simulated head","Diff"'
    !!!    write(itmp2,'(a)') 'ZONE T="data"'
    !!!
    !!!    ! correlation coefficient r2
    !!!    ! observed ybar and simulated fbar means
    !!!    ! sserr sum of squares
    !!!    ybar=0.0
    !!!    fbar=0.0
	   !!! sserr=0.0
	   !!! nobs_head_found=0
	   !!! do i=1,nobs_head
	   !!!
	   !!!     if(obs_el(i) == -1) cycle  ! -1 indicates obs point was outside of domain
    !!!
    !!!        if(	xob(i) < xmin_trunc_h .or. xob(i) > xmax_trunc_h .or. &    ! don't use points if outside truncate limits
	   !!!         yob(i) < ymin_trunc_h .or. yob(i) > ymax_trunc_h .or. &
	   !!!         zob(i) < zmin_trunc_h .or. zob(i) > zmax_trunc_h         ) cycle
    !!!
		  !!!  if(nln == 8) then   
		  !!!      ! interpolate head using Fabiens routine
    !!!            do j=1,8
	   !!!             nodal_function(j) = medium.Head(medium.in(j,i))
	   !!!             basis_function(j) = basisfunctions(i,j)
	   !!!         end do
    !!!            head_interp=dFE_interp(nodal_function,basis_function) 
    !!!    
    !!!        else  ! for now use nearest node
		  !!!      call find_node(xob(i),yob(i),zob(i),nde,medium)
    !!!            head_interp=medium.Head(nde)
    !!!        end if
    !!!
    !!!
    !!!   	    nobs_head_found = nobs_head_found + 1
		  !!!  ybar=ybar+hob(i)
		  !!!  fbar=fbar+head_interp
	   !!!     sserr=sserr+(hob(i)-head_interp)**2
    !!!           
		  !!!  write(itmp,*) hob(i),head_interp
		  !!!
		  !!!  write(itmp2,'(6e20.8)') xob(i),yob(i),zob(i),hob(i),head_interp,head_interp-hob(i)
    !!!
		  !!!  hmin=min(hmin,hob(i),head_interp)
		  !!!  hmax=max(hmax,hob(i),head_interp)
    !!!
	   !!! end do
	   !!!
	   !!! ybar=ybar/nobs_head_found
	   !!! fbar=fbar/nobs_head_found
	   !!!
	   !!! ! sum of squares total
	   !!! sstot=0.0
	   !!! do i=1,nobs_head
	   !!!     if(obs_el(i) /= -1) sstot=sstot+(hob(i)-ybar)**2
	   !!! end do
	   !!! r2=1.0-sserr/sstot
    !!!
	   !!! write(itmp,'(a)') 'zone t="straight line"' 
	   !!! write(itmp,*) hmin,  hmin
	   !!! write(itmp,*) hmax,  hmax
    !!!
	   !!! write(itmp,'(a,f10.4,a)') 'zone t="R<sup>2</sup> =',r2,'"' 
	   !!! write(itmp,*) r2,  r2
    !!!
	   !!! call freeunit(itmp)
	   !!! call freeunit(itmp2)
    !!!
    !!!end subroutine compare_heads_output
    !!!!------------------------------------------------------------------------
    !!!SUBROUTINE obs_points_shape(medium)
    !!!    !------------------------------------------------
    !!!    !.....Code to evaluate the shape function values
    !!!    !.....at some input points, given their global
    !!!    !.....coordinates {X,Y,Z} in the cartesian space.
    !!!    !------------------------------------------------
    !!!
    !!!    ! Rob McLaren hardwired this for 8-node blocks for Andra
    !!!
    !!!    !USE GW_buffer
    !!!    !USE fef, ONLY : br8shp,         &
    !!!    !                quad4shp,       &
    !!!    !                tria3shp_areal, &
    !!!    !                pipe2shp,       &
    !!!    !                Element_nDOF,   &
    !!!    !                element_dim
    !!!    implicit none
    !!!
    !!!    type(HGSTecplotdomain) medium
    !!!
    !!!
    !!!    INTEGER  :: nod, eld, nDOF, IERR, iunit
    !!!    INTEGER(di) :: i, j, k
    !!!    REAL(dr) :: gcoo(3), lcoo(3)
    !!!    REAL(dr) :: coo(3,8)
    !!!    real(dr) :: xyz_min(3), xyz_max(3)
    !!!
    !!!    logical :: display=.true.
    !!!
    !!!    logical :: have_shape_file=.false.
    !!!
    !!!    logical :: obs_pt_found
    !!!
    !!!    !------------------------------------------------
    !!!
    !!!    ! check if shape functions have already been computed for observation points)
    !!!    inquire(FILE=TRIM(LocalPrefix) // "o.ObsShape.txt",exist=have_shape_file)
    !!!    if(have_shape_file) then
    !!!        call getunit(itmp)
    !!!        OPEN(iunit, FILE=TRIM(LocalPrefix) // "o.ObsShape.txt")
    !!!        do i=1,nobs_head
    !!!            read(iunit, FMT=100) obs_el(i), (BasisFunctions(i,j),j=1,8)  
    !!!        end do
    !!!        call freeunit(itmp)
    !!!        return
    !!!    end if
    !!!
    !!!    IF(DISPLAY)THEN
    !!!       PRINT *, "Evaluating shape functions at XYZ observation locations"
    !!!       PRINT *, "This might take several minutes..."
    !!!    END IF
    !!!    write(ieco, FMT=*)" Evaluating shape functions at XYZ observation locations"
    !!!
    !!!    call getunit(itmp)
    !!!    OPEN(iunit, FILE=TRIM(LocalPrefix) // "o.ObsShape.txt")
    !!!
    !!!    ! - - - Observation points loop:
    !!!    obs_loop : &
    !!!    DO j = 1, nobs_head
    !!!
    !!!        call Msg( j,' of',nobs_head,' observation points'
    !!!
    !!!        obs_pt_found=.false.
    !!!
    !!!        ! - Get coordinates of current point:
    !!!        gcoo(1) = xob(j)
    !!!        gcoo(2) = yob(j)
    !!!        gcoo(3) = zob(j)
    !!!    
    !!!        !.....Element loop:
    !!!        elm_loop : &
    !!!        DO i = 1, medium.Ne
    !!!            nod = 128  ! brick
    !!!            eld = 3    ! elem dim
    !!!            nDOF = 8   ! # of nodes 
    !!!   
    !!!            DO k = 1, nDOF
    !!!              coo(1,k) = medium.X(medium.IN(k,i))
    !!!              coo(2,k) = medium.Y(medium.IN(k,i))
    !!!              coo(3,k) = medium.Z(medium.IN(k,i))
    !!!            END DO
    !!!        
    !!!
    !!!            DO k = 1, 3
    !!!                xyz_min(k) = MINVAL(coo(k,1:nDOF))
    !!!                xyz_max(k) = MAXVAL(coo(k,1:nDOF))
    !!!            END DO
    !!!  
    !!!   
    !!!            ! - Test its position in current element:
    !!!            SELECT CASE (nod)
    !!!            CASE (128)
    !!!                call get_rst_from_xyz(nod,3,gcoo(1:3),xyz_min,xyz_max,lcoo,ierr)
    !!!                ! - Found it!!!
    !!!                IF(IERR == 0)THEN
    !!!
    !!!                    obs_pt_found=.true.
    !!!                    SELECT CASE (nod)
    !!!                    CASE (128)
    !!!                        CALL br8shp(1,lcoo(1:3),BasisFunctions(j,1:nDOF))
    !!!                        obs_el(j)=i
    !!!                    END SELECT
    !!!
    !!!                    WRITE(iunit, FMT=100)obs_el(j), BasisFunctions(j,1:nDOF)  
    !!!                    100 FORMAT(i7,1X,9(1X,g18.11))
    !!!                    EXIT elm_loop
    !!!                END IF
    !!!            END SELECT
    !!!        END DO &
    !!!        elm_loop
    !!!    
    !!!
    !!!        if(.not. obs_pt_found) WRITE(iunit, FMT=100)-1, -1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.  
    !!!    END DO &
    !!!   obs_loop
    !!!
    !!!
    !!!    call freeunit(itmp)
    !!!
    !!!END SUBROUTINE obs_points_shape
    !!!
    !!!SUBROUTINE br8shp(igp,gpval,sfun)
    !!!    !------------------------------------------------------------------------
    !!!    !.....Code to compute the shape functions and their derivatives for 
    !!!    !.....8-noded brick elements.
    !!!    !------------------------------------------------------------------------
    !!!    !                                  t   s   
    !!!    !                                 ¦  /  
    !!!    !                            4*---¦-/--*3     
    !!!    !                           /¦    ¦/  /¦    
    !!!    !                          / ¦    /  / ¦-----> r  
    !!!    !                        1*--¦------*2 ¦   
    !!!    !                         ¦ 8*------¦--*7 
    !!!    !                         ¦ /       ¦ /
    !!!    !                         ¦/        ¦/
    !!!    !                        5*---------*6
    !!!    !------------------------------------------------------------------------
    !!!    !---INPUT---
    !!!    ! igp   = current Gauss point identifier
    !!!    ! gpval = Gauss point coordinates
    !!!    !---OUTPUT---
    !!!    ! sfun  = shape functions vector 
    !!!    ! dsfun = local derivatives matrix
    !!!    !------------------------------------------------------------------------
    !!!    IMPLICIT NONE
    !!!    INTEGER,  INTENT(IN)  :: igp
    !!!    REAL(dr), INTENT(IN)  :: gpval(3)
    !!!    REAL(dr), INTENT(OUT) :: sfun(8)
    !!!    REAL(dr)  ::  r, s, t, rp, rm, sp, sm, tp, tm
    !!!
    !!!    REAL*8 :: linf
    !!!
    !!!    !------------------------------------------------------------------------
    !!!
    !!!    !.....Prepare coordinates:
    !!!    r = gpval(igp  )
    !!!    s = gpval(igp+1)
    !!!    t = gpval(igp+2)
    !!!    rp = 2.00_dr * linf( r)
    !!!    rm = 2.00_dr * linf(-r)
    !!!    sp = 2.00_dr * linf( s)
    !!!    sm = 2.00_dr * linf(-s)
    !!!    tp = 0.25_dr * linf( t)
    !!!    tm = 0.25_dr * linf(-t)
    !!!      
    !!!    !.....Shape functions:
    !!!    sfun(1) = rm * sm * tm
    !!!    sfun(2) = rp * sm * tm
    !!!    sfun(3) = rp * sp * tm
    !!!    sfun(4) = rm * sp * tm
    !!!    sfun(5) = rm * sm * tp
    !!!    sfun(6) = rp * sm * tp
    !!!    sfun(7) = rp * sp * tp
    !!!    sfun(8) = rm * sp * tp
    !!!       
    !!!    !.....Local derivatives:
    !!!
    !!!    RETURN
    !!!
    !!!END SUBROUTINE br8shp
    !!!subroutine get_rst_from_xyz(code,mdim,xyz,xyz_min,xyz_max,xlocal,ierr)
    !!!    !-------------------------------------------------------------
    !!!    !.....Code to find the local coordinates given the global xyz.
    !!!    !-------------------------------------------------------------
    !!!    !---INPUT---
    !!!    ! code    = element type code
    !!!    ! mdim    = model dimension
    !!!    ! xyz     = Cartesian coordinates of the input points
    !!!    ! xyz_min = minimum coordinates of the element nodes
    !!!    ! xyz_max = maximum coordinates of the element nodes
    !!!    !---OUTPUT---
    !!!    ! IERR /= 0 means that the point is outside the element
    !!!    !-------------------------------------------------------------
    !!!
    !!!    IMPLICIT NONE
    !!!    INTEGER,  INTENT(IN)  :: code
    !!!    INTEGER,  INTENT(IN)  :: mdim
    !!!    REAL(dr), INTENT(IN)  :: xyz(mdim)
    !!!    REAL(dr), INTENT(IN)  :: xyz_min(mdim)
    !!!    REAL(dr), INTENT(IN)  :: xyz_max(mdim)
    !!!    INTEGER  :: i, j, ierr
    !!!    REAL(dr) :: num, den
    !!!    REAL(dr) :: xlocal(mdim)
    !!!
    !!!    REAL(dr), PARAMETER :: tol2 = 1.0E-05_dr ! 1.0E-03_dr
    !!!
    !!!    !-------------------------------------------------------------
    !!!
    !!!    IERR = 0
    !!!
    !!!    SELECT CASE (code)
    !!!    !.....Line, quadrilateral, or hexahedral:
    !!!      CASE (128)
    !!!    ! - - - Apply geometrical formula:
    !!!        !xlocal = ( 2.0_dr * xyz - ( xyz_max + xyz_min ) ) / ( xyz_max - xyz_min )
    !!!        j = 0
    !!!        DO i = 1, mdim
    !!!           num = 2.0_dr * xyz(i) - ( xyz_max(i) + xyz_min(i) )
    !!!           den = xyz_max(i) - xyz_min(i)
    !!!           IF(den == 0.0_dr)CYCLE
    !!!           j = j + 1
    !!!           xlocal(j) = num / den
    !!!           IF(ISNaN(xlocal(j)))xlocal(j) = 0.0_dr
    !!!        END DO
    !!!        IF( ANY(ABS(xlocal(1:mdim)) > (1.0_dr+5.0_dr*tol2)) )IERR = -1
    !!!    END SELECT
    !!!
    !!!END subroutine get_rst_from_xyz
    !!!
    !!!real*8 FUNCTION linf(x) 
    !!!    !--------------------------------------
    !!!    IMPLICIT NONE
    !!!    REAL(dr), INTENT(IN) :: x
    !!!    
    !!!    !--------------------------------------
    !!!    linf = 0.5_dr * ( 1.0_dr + x )
    !!!    !--------------------------------------
    !!!END FUNCTION linf
    !!!
    !!!!-------------------------------------------------------------------------------!
    !!!!    -----------------------------------------------------------------------    !
    !!!!                I n t e r p o l a t i o n     L i b r a r y                    !
    !!!!    -----------------------------------------------------------------------    !
    !!!!-------------------------------------------------------------------------------!
    !!!
    !!!FUNCTION dFE_interp(nfun,sfun) 
    !!!    !---------------------------------------
    !!!    !.....Routine to interpolate a function
    !!!    !.....at a point of an element, given 
    !!!    !.....the values of the shape functions
    !!!    !.....at that point and the nodal values
    !!!    !.....of that function.
    !!!    !---------------------------------------
    !!!    !---INPUT---
    !!!    ! nfun = nodal values of the function
    !!!    ! sfun = values of the shape functions
    !!!    !---------------------------------------
    !!!    IMPLICIT NONE
    !!!    REAL(dr)  :: nfun(8)
    !!!    REAL(dr)  :: sfun(8)
    !!!    REAL(dr) :: dfe_interp
    !!!    !---------------------------------------
    !!!
    !!!    dFE_interp = DOT_PRODUCT(nfun, sfun)
    !!!
    !!!END FUNCTION dFE_interp

    !*************************************************************************old plot_data routines 
   
    !*************************************************************************old plot_data routines 
    !------------------------------------------------------------------------
    subroutine TitleRead
         
	    implicit none

   !inquire(file = trim(LocalPrefix)//'o.gen', exist=TitleFileExists)
	    !if(.not. TitleFileExists) then
	    !    call Msg('No title(.gen) file: '//trim(LocalPrefix)//'o.gen')
	    !    stop
	    !else
    	!    call Msg('Title(.gen) file: '//trim(LocalPrefix)//'o.gen')
	    !end if
     !
     !   ! read title
	    !OPEN(56,file = trim(LocalPrefix)//'o.gen', &
		   ! status = 'old',  &
		   ! action = 'read', &
		   ! form   = 'unformatted', &
		   ! iostat = status)
	    !if(status /= 0) then
		   ! call Msg('FILE ERROR: '//trim(LocalPrefix)//'o.gen')
		   ! stop
	    !end if
	    !read(56) ntit
        
        ntit=1
	    allocate(title(ntit), stat=status)
	    if(status /= 0) then
		    call Msg('ALLOCATION ERROR: Title')
		    stop
	    end if
	    title = 'testing hsplot 2012'  ! automatic initialization
	    !read(56) (title(i),i=1,ntit)
     !   do i=1,ntit
	    !    call Msg(trim(title(i)))
     !   end do
     !
	    !close(56)

    end subroutine TitleRead
    !------------------------------------------------------------------------
    subroutine SpeciesRead
         
	    implicit none

        integer :: i
        
        integer :: Fnum
        character(MAX_STR) :: FName
        character(MAX_STR) :: line

        inquire(file = trim(LocalPrefix)//'o.species', exist=SpeciesFileExists)
        if(.not. SpeciesFileExists) then
            call Msg( 'No species file present')
            return
        else
	        call Msg( 'Species file: '//trim(LocalPrefix)//'o.species')
        end if
       
        FName='scratchlist'
        inquire(File=FName,Exist=FileExists)
        if(FileExists) i=system('del scratchlist')
        
        TmpSTR='dir /b *o.conc_pm.* > '//trim(FName)
        i=system(TmpSTR)
        
        
        call openascii(FNum,FName)
        write(TMPstr,NFSFormat) 1
        CountSpecies: do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) then
                exit
            end if
            if(index(line,trim(TMPstr)) > 1) then
                nspeciesmob=nspeciesmob+1
            end if
        end do CountSpecies
        
        write(TMPStr,*) nspeciesmob
        call Msg('Number of mobile species: '//trim(TMPStr))
            
	    allocate(spname(nspeciesmob),stat=status)
        
        rewind(FNum)
        i=0
        write(TMPstr,NFSFormat) 1
        NameSpecies: do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) then
                exit
            end if
            l2=index(line,trim(TMPStr))
            if(l2 > 1) then
                i=i+1 
                l1=index(line,'o.conc_pm.')
                spname(i)=line(l1+10:l2-2)
                call Msg('Species name: '//trim(spname(i)))
            end if
        end do NameSpecies

        

    end subroutine SpeciesRead
    !----------------------------------------------------------------------
    subroutine enter_prefix(prefix,lp,nunit,ext)
      logical batch_ex,np_ex
      character(*) :: prefix
      character*(*) ext
      character*10 dext
!rt-nov99
      character(1000) :: fname
      integer l_ext,nunit,lp


      dext=ext
      l_ext=index(dext,' ')+1

      inquire(file='batch.pfx',exist=batch_ex)

133   if(batch_ex) then
        open(nunit,file='batch.pfx',status='unknown')
        read(nunit,132,err=136) prefix
        goto 137

136     write(*,*) 'ERROR READING PREFIX IN BATCH.PFX '
        write(*,*) '  Switching to interactive mode '
        write(*,*) '  Try again...'
        batch_ex=.false.
        goto 133

137     close(nunit)
	else
        write(*,*) ' Give prefix of problem filename...'
        read(*,132) prefix
132     format(a32)
      end if

      lp=index(prefix,' ')-1
      if(lp.eq.-1 .or. lp.gt.40) then
        lp=40
      else if(lp.eq.0) then
        if(batch_ex) then    ! bad batch.pfx file so switch to interactive
          write(*,*)'EMPTY PREFIX IN BATCH.PFX '
          write(*,*)'  Switching to interactive mode '
          write(*,*)'  Try again...'
          batch_ex=.false.
        else
          write(*,*) 'Empty prefix - try again'
        end if
        goto 133
      else

        fname =  prefix(:lp)//dext(:l_ext)
!rt-nov99        inquire(file=prefix(:lp)//dext(:l_ext),exist=np_ex)
        inquire(file=fname,exist=np_ex)
        if(.not. np_ex) then
          if(batch_ex) then    ! bad batch.pfx file so switch to interactive
            write(*,*)'COULD NOT FIND FILE: ',prefix(:lp)//dext(:l_ext)
            write(*,*)'  Switching to interactive mode '
            write(*,*)'  Try again...'
            batch_ex=.false.
          else
            write(*,*)'COULD NOT FIND FILE: ',prefix(:lp)//dext(:l_ext)
            write(*,*)'  Try again..'
          end if
          goto 133
        else
!rt-nov99          open(nunit,file=prefix(:lp)//dext(:l_ext),
          open(nunit,file=fname,status='unknown',err=134)
          goto 135
134       if(batch_ex) then    ! bad batch.pfx file so switch to interactive
            write(*,*)'COULD NOT OPEN: ',prefix(:lp)//dext(:l_ext)
            write(*,*)'  Switching to interactive mode '
            write(*,*)'  Try again...'
            batch_ex=.false.
	    else
            write(*,*)'COULD NOT OPEN: ',prefix(:lp)//dext(:l_ext)
            write(*,*)'  Try again...'
	    end if
          goto 133
        end if
      end if

135   continue

      return
end subroutine enter_prefix

    !----------------------------------------------------------------------
	subroutine write_tecplot_array2(array,nvar,nmax,use_array,description,truncate,iunit,iMed)

	implicit none

    integer :: i, j, nmax, iunit
	real :: array(nmax)
    integer :: nvar
	logical :: use_array(nmax)
    integer :: iMed
    character*(*) :: description
	logical :: truncate
    integer(c_int64_t) :: iMax_l
    integer :: nUsed
    
    If(BinaryMode) then
        if(allocated(FloatValues)) deallocate(FloatValues)
        nUsed=0
		do j=1,nmax
			if(use_array(j)) nUsed=nUsed+1  
		end do
        allocate(FloatValues(nUsed))
        FloatValues(:)=0.0d0
	    
        if(truncate) then
            i=0
		    do j=1,nmax
			    if(use_array(j)) then
                    i=i+1
                    FloatValues(i)=array(j)  
                end if
            end do
	    else
		    FloatValues(:)=array(:)  
        end if

        imax_l=nUsed
        call Msg(description)
        i = tecZoneVarWriteFloatValues(OutputFileHandle_M(IMed), &
            outputZone, nvar, 0, imax_l, FloatValues)
    
    else
	    write(iunit,'(a)') description
	    if(truncate) then
		    do j=1,nmax
			    if(use_array(j)) write(iunit,'(5g26.17e3)') array(j)
		    end do
	    else
		    write(iunit,'(5g26.17e3)') (array(j),j=1,nmax)
        end if
    end if

	end subroutine write_tecplot_array2
    !----------------------------------------------------------------------
	subroutine write_tecplot_array(array,nmax,use_array,description,truncate,iunit)

	implicit none

    integer :: j, nmax, iunit
	real :: array(nmax)
	logical :: use_array(nmax)
	character*(*) :: description
	logical :: truncate


	write(iunit,'(a)') description
	if(truncate) then
		do j=1,nmax
			if(use_array(j)) write(iunit,'(5g26.17e3)') array(j)
		end do
	else
		write(iunit,'(5g26.17e3)') (array(j),j=1,nmax)
	end if

	end subroutine write_tecplot_array
    !----------------------------------------------------------------------
	subroutine write_tecplot_iarray2(array,nvar,nmax,use_array,description,truncate,iunit,iMed)

	implicit none

    integer :: i, j, nmax, iunit
	integer :: array(nmax)
    integer :: nvar
	logical :: use_array(nmax)
    integer :: iMed
	character*(*) :: description
	logical :: truncate
    integer(c_int64_t) :: iMax_l
    integer :: nUsed
    

    If(BinaryMode) then
        if(allocated(int32Values)) deallocate(int32Values)
        
        nUsed=0
		do j=1,nmax
			if(use_array(j)) nUsed=nUsed+1  
		end do

        allocate(int32Values(nUsed))
        int32Values(:)=0
	    if(truncate) then
            i=0
		    do j=1,nmax
			    if(use_array(j)) then
                    i=i+1
                    int32Values(i)=array(j)  
                end if
            end do
	    else
		    int32Values(:)=array(:)  
        end if
        
        imax_l=nUsed
         call Msg(description)
        i = tecZoneVarWriteint32Values(OutputFileHandle_M(IMed), &
            outputZone, nvar, 0, imax_l, int32Values)

    else

	    write(iunit,'(a)') description
	    if(truncate) then
		    do j=1,nmax
			    if(use_array(j)) write(iunit,'(5i10)') array(j)
		    end do
	    else
		    write(iunit,'(5i10)') (array(j),j=1,nmax)
        end if
    end if

	end subroutine write_tecplot_iarray2
    !----------------------------------------------------------------------
	subroutine write_tecplot_iarray(array,nmax,use_array,description,truncate,iunit)

	implicit none

    integer :: j, nmax, iunit
	integer :: array(nmax)
	logical :: use_array(nmax)
	character*(*) :: description
	logical :: truncate


	write(iunit,'(a)') description
	if(truncate) then
		do j=1,nmax
			if(use_array(j)) write(iunit,'(5i10)') array(j)
		end do
	else
		write(iunit,'(5i10)') (array(j),j=1,nmax)
	end if

	end subroutine write_tecplot_iarray
    !----------------------------------------------------------------------
	subroutine write_tecplot_darray2(array,nvar,nmax,use_array,description,truncate,iunit,iMed)

	implicit none

    integer :: i, j, nmax, iunit
	real(dr) :: array(nmax)
    integer :: nvar
	logical :: use_array(nmax)
    integer :: iMed
	character*(*) :: description
	logical :: truncate
    integer(c_int64_t) :: iMax_l
    integer :: nUsed

    If(BinaryMode) then
        if(allocated(DoubleValues)) deallocate(DoubleValues)
        nUsed=0
		do j=1,nmax
			if(use_array(j)) nUsed=nUsed+1  
		end do

        allocate(DoubleValues(nUsed))
        DoubleValues(:)=0.0d0
        
        i=0
	    if(truncate) then
		    do j=1,nmax
			    if(use_array(j)) then
                    i=i+1  
                    DoubleValues(i)=array(j)  
                    end if
		    end do
	    else
		    DoubleValues(:)=array(:)  
        end if

        imax_l=nUsed
        call Msg(description)
        i = tecZoneVarWriteDoubleValues(OutputFileHandle_M(IMed), &
            outputZone, nvar, 0, imax_l, DoubleValues)

    else
    
	    write(iunit,'(a)') description
	    if(truncate) then
		    do j=1,nmax
			    if(use_array(j)) write(iunit,'(5g26.17e3)') array(j)
		    end do
	    else
		    write(iunit,'(5g26.17e3)') (array(j),j=1,nmax)
        end if
    end if
    
    

	end subroutine write_tecplot_darray2
    !----------------------------------------------------------------------
	subroutine write_tecplot_darray(array,nmax,use_array,description,truncate,iunit)

	implicit none

    integer :: j, nmax, iunit
	real(dr) :: array(nmax)
	logical :: use_array(nmax)
	character*(*) :: description
	logical :: truncate

	write(iunit,'(a)') description
	if(truncate) then
		do j=1,nmax
			if(use_array(j)) write(iunit,'(5g26.17e3)') array(j)
		end do
	else
		write(iunit,'(5g26.17e3)') (array(j),j=1,nmax)
    end if

    end subroutine write_tecplot_darray
    !------------------------------------------------------------------------
    subroutine TecplotVarAdd(varname,gnvar,vnvar,gvarlist)
	    implicit none
    	
	    integer :: gnvar, vnvar
	    character(*) :: varname
	    character(512) :: gvarlist

	    if(gnvar==0) then
            if(BinaryMode) then
	            gvarlist=trim(varname)
            else
	            gvarlist='VARIABLES = "'//trim(varname)//'"'
            end if
	    else
            if(BinaryMode) then
	            gvarlist=trim(gvarList)//','//trim(varname)
            else
	            gvarlist=trim(gvarList)//',"'//trim(varname)//'"'
            end if
	    end if
	    gnvar=gnvar+1
	    write(*,'(i4,5x,a)') gnvar,trim(varname)
	    vnvar=gnvar
    end subroutine TecplotVarAdd
    !------------------------------------------------------------------------
    subroutine init_var8(np,fname,var,varname,vnvar,gnvar,gvarlist)
	    implicit none

	    integer :: np, vnvar, gnvar
	    character(*) :: fname, varname
	    real(dr) :: var(np)
	    character(512) :: gvarlist

        call TecplotVarAdd(varname,gnvar,vnvar,gvarlist)
        TBinary_vartype(gnvar)=2  ! double precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=1 ! node-centred
        TBinary_shareVarFromZone(gnvar)=0
	    call read_var8(np,fname,var)

    end subroutine init_var8
    !------------------------------------------------------------------------
    subroutine read_var8(np,fname,var)
	     
	    implicit none

	    integer :: j, np
	    character(*) :: fname
	    real(dr) :: var(np)

	    call getunit(itmp)
	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
	    if(status /= 0) then
		    call Msg('read_var8 FILE ERROR: '//fname)
		    stop
	    end if
	    read(itmp) message
	    read(itmp) (var(j),j=1,np)
	    call freeunit(itmp)

    end subroutine read_var8
    !------------------------------------------------------------------------
    subroutine init_var4(np,fname,var,varname,vnvar,gnvar,gvarlist)
	    implicit none

	    integer :: np, vnvar, gnvar
	    character(*) :: fname, varname
	    real :: var(np)
	    character(512) :: gvarlist

        call TecplotVarAdd(varname,gnvar,vnvar,gvarlist)
        TBinary_vartype(gnvar)=1  !single precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=1 ! node-centred
        TBinary_shareVarFromZone(gnvar)=0
	    call read_var4(np,fname,var)

    end subroutine init_var4
    !------------------------------------------------------------------------
    subroutine read_var4(np,fname,var)
	     
	    implicit none

	    integer :: j, np
	    character(*) :: fname
	    real :: var(np)

	    call getunit(itmp)
	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
	    if(status /= 0) then
		    call Msg('read_var4 FILE ERROR: '//fname)
		    stop
	    end if
	    read(itmp) message
	    read(itmp) (var(j),j=1,np)
	    call freeunit(itmp)
    end subroutine read_var4

    !------------------------------------------------------------------------
    subroutine init_vec4(np,fname,var_x, name_x, nvar_x, &
							      var_y, name_y, nvar_y, &
							      var_z, name_z, nvar_z,gnvar,gvarlist)
	    implicit none

	    integer :: np, gnvar
	    character(*) :: fname
	    character(*) ::  name_x, name_y, name_z 
	    real :: var_x(np), var_y(np), var_z(np)
	    integer :: nvar_x, nvar_y, nvar_z
	    character(512) :: gvarlist

        call TecplotVarAdd(name_x,gnvar,nvar_x,gvarlist)
        TBinary_vartype(gnvar)=1  ! single precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

        call TecplotVarAdd(name_y,gnvar,nvar_y,gvarlist)
        TBinary_vartype(gnvar)=1  ! single precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

        call TecplotVarAdd(name_z,gnvar,nvar_z,gvarlist)
        TBinary_vartype(gnvar)=1  ! single precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

	    call read_vec4(np,fname,var_x, var_y, var_z)

    end subroutine init_vec4
    !------------------------------------------------------------------------
    subroutine read_vec4(np,fname,var_x, var_y, var_z)
	     
	    implicit none

	    integer :: j, np
	    character(*) :: fname
	    real :: var_x(np), var_y(np), var_z(np)

	    call getunit(itmp)
	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
	    if(status /= 0) then
		    call Msg('read_vec4 FILE ERROR: '//fname)
		    stop
	    end if
	    read(itmp) message
	    do j=1,np
		    read(itmp) var_x(j),var_y(j),var_z(j)
	    end do
	    call freeunit(itmp)
    end subroutine read_vec4
    !------------------------------------------------------------------------
    subroutine init_vec8(np,fname,var_x, name_x, nvar_x, &
							      var_y, name_y, nvar_y, &
							      var_z, name_z, nvar_z,gnvar,gvarlist)
	    implicit none

	    integer :: np, gnvar
	    character(*) :: fname
	    character(*) ::  name_x, name_y, name_z 
	    real(dr) :: var_x(np), var_y(np), var_z(np)
	    integer :: nvar_x, nvar_y, nvar_z
	    character(512) :: gvarlist

        call TecplotVarAdd(name_x,gnvar,nvar_x,gvarlist)
        TBinary_vartype(gnvar)=2  ! double precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

        call TecplotVarAdd(name_y,gnvar,nvar_y,gvarlist)
        TBinary_vartype(gnvar)=2  ! double precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

        call TecplotVarAdd(name_z,gnvar,nvar_z,gvarlist)
        TBinary_vartype(gnvar)=2  ! double precision
        TBinary_passiveVarList(gnvar)=0
        TBinary_valueLocation(gnvar)=0 ! cell-centred
        TBinary_shareVarFromZone(gnvar)=0

	    call read_vec8(np,fname,var_x, var_y, var_z)

    end subroutine init_vec8
    !------------------------------------------------------------------------
    subroutine read_vec8(np,fname,var_x, var_y, var_z)
	     
	    implicit none

	    integer :: j, np
	    character(*) :: fname
	    real(dr) :: var_x(np), var_y(np), var_z(np)

	    call getunit(itmp)
	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
	    if(status /= 0) then
		    call Msg('read_vec8 FILE ERROR: '//fname)
		    stop
	    end if
	    read(itmp) message
	    do j=1,np
		    read(itmp) var_x(j),var_y(j),var_z(j)
	    end do
	    call freeunit(itmp)
    end subroutine read_vec8
    !------------------------------------------------------------------------
    subroutine GetTimeStamp(fext,medium,OutputExists)
	     
	    implicit none
    	
	    character(100) :: fname
	    character(*) :: fext
	    logical :: lfile
        type(HGSTecplotdomain) medium
        logical :: OutputExists
        real(dr) :: TInit

	    message='0.0'
	    timestamp=message

        OutputExists=.false.
	    
	    ! always use timestamp from first conc file if present
	    if(SpeciesFileExists) then
		    
		    fname=trim(LocalPrefix)//'o.conc_'//trim(medium.name)//'.'//trim(spname(1))//fext
		    inquire(file=fname,exist=lfile)
		    if(lfile) then
	            OutputExists=.true.
			    call getunit(itmp)
	            open(itmp,file=fname,status='old',action = 'read',form='unformatted',iostat=status)
			    if(status /= 0) then
				    call Msg('GetTimeStamp FILE ERROR: '//fname)
				    stop
			    end if
			    read(itmp) message
			    call freeunit(itmp)

			    timestamp=message
                read(timestamp,*) TInit
                Tinit=TInit+TecplotTimeOffset
                write(Timestamp,*) TInit

			    return
		    end if
	    end if
    	
	    fname=trim(LocalPrefix)//'o.head_'//trim(medium.name)//fext
	    inquire(file=trim(fname),exist=lfile)
	    if(lfile) then
	        OutputExists=.true.
		    call getunit(itmp)
	        open(itmp,file=fname,status='old',action = 'read',form='unformatted')
!		    if(status /= 0) then
!			    call Msg('FILE ERROR: '//fname)
!			    stop
!		    end if
		    read(itmp) message
    	
		    timestamp=message
            read(timestamp,*) TInit
            Tinit=TInit+TecplotTimeOffset
            write(Timestamp,*) TInit

		    call freeunit(itmp)
	    end if


    end subroutine GetTimeStamp
    !------------------------------------------------------------------------
   subroutine BuildCustomZoneNames(medium, global)
   
        implicit none
        
        integer :: i
        
        integer :: FnumEco
        character(MAX_STR) :: FNameEco

        type(HGSTecplotdomain) medium
        type(HGSTecplotdomain) global
        
        character(MAX_LBL) :: line
        integer :: i1
        integer :: iZone
        character(MAX_LBL) :: ZoneName
        character(MAX_LBL) :: DomainStr
        
        integer :: newIchar
        
        real(dr) :: TotOlfArea
        real(dr) :: TotOlfXYArea
        real(dr) :: TotOlfZoneArea
        real(dr) :: TotOlfZoneXYArea
        real(dr) :: TriangleArea
        
        ! open the local eco file
        FNameEco=trim(LocalPrefix)//'o.eco'
        inquire(file=FNameEco , exist=FileExists)
        if(.not. FileExists) then
            call Msg('No eco file to build customlabels: '//trim(FNameEco))
        end if
       
        call OpenAscii(FnumEco,FNameEco)
        call Msg( 'HGS eco file: '//trim(FNameEco))

        ! scan for GROK: y vertical
        yVertical=.false.
        do
            read(FNumEco,'(a)',iostat=status) line
            if(status/=0) then
                exit
            end if
            
            ! write(*,'(a)') trim(line)
            
            if(index(line,'GROK: y vertical')>0) then
                yVertical=.true.
                exit
            end if
        end do
        
        rewind(FNumEco)

        continue

      
        
        ! set up the search string
        Wells=.false.
        FindSecondOccurrence=.false.
        select case (trim(medium.name))
        case ('pm')
            DomainStr='OROUS MEDIA DOMAIN PROPERTIES'
        case ('dual')
            DomainStr='UAL CONTINUUM PROPERTIES'        
        case ('frac')
            DomainStr='RACTURE DOMAIN PROPERTIES'
        case ('olf')
            DomainStr='URFACE DOMAIN PROPERTIES'
        case ('chan')
            DomainStr='HANNEL FLOW PROPERTIES'
        case ('well')
            DomainStr='ELL FLOW PROPERTIES'
            Wells=.true.
        case ('tile')
            ! HGS eco file labels tile properties section incorrectly as WELL FLOW PROPERTIES
            if(.not. wells) FindSecondOccurrence=.true.
            DomainStr='ELL FLOW PROPERTIES'
        end select


        ! scan for search string
        do
            read(FNumEco,'(a)',iostat=status) line
            if(status/=0) then
                if(FindSecondOccurrence) then
                    FindSecondOccurrence=.false.
                else
                    exit
                end if
            end if
            
            ! write(*,'(a)') trim(line)
            
            if(index(line,trim(DomainStr))>0) exit
        end do

        continue

        
        Domain_: do
            read(FNumEco,'(a)',iostat=status) line
            if(status/=0) exit
            
            i1=index(line,'----------------------')
            if (i1>0) exit Domain_
            
            i1=index(line,'ZONE:')
            if(i1>0) then  
                Zone: do
                    read(line(i1+6:),*) iZone
                    Material: do
                        read(FNumEco,'(a)') line
                        i1=index(line,'MATERIAL:')
                        if(i1>0) then
                            select case(medium.name)
                            case ('well')
                                ZoneName=line(i1+10:)
                            case ('olf')
                                ZoneName=line(i1+10:)
                                if(medium.in(4,1)==0) then 
                                    call Msg('SW zone areas written to: SWAreas.csv')
                                    if(izone==1) then ! open csv file for olf areas
                                        Outputfilename='SWAreas.csv'
                                        call OpenAscii(itmp,Outputfilename)
                                        write(itmp,'(a)') ' Zone#, XYZ Area,Projected XY Area, ZoneName'
                                        ! calculate total domain area and call it zone 0, Domain
                                        TotOlfArea=0.0d0
                                        TotOlfXYArea=0.0d0
                                        TotOlfZoneArea=0.0d0
                                        TotOlfZoneXYArea=0.0d0
                                        do i=1,medium.NE
                                            !do j=1,medium.NLN
                                            !    write(*,*) j,i,medium.in(j,i)
                                            !    write(*,*) global.x(medium.in(j,i))
                                            !end do
                                        
                                            continue
                                            ! Calculate olf element true area 
                                            call area_triangle(global.x(medium.in(1,i)),global.y(medium.in(1,i)),global.z(medium.in(1,i)), &
                                                    &           global.x(medium.in(2,i)),global.y(medium.in(2,i)),global.z(medium.in(2,i)), &
                                                    &           global.x(medium.in(3,i)),global.y(medium.in(3,i)),global.z(medium.in(3,i)), &
                                                    &           TriangleArea)
                                        
                                            TotOlfArea=TotOlfArea+TriangleArea
                                            if(medium.ZoneNum(i)==iZone) TotOlfZoneArea=TotOlfZoneArea+TriangleArea 

                                            call area_triangle(global.x(medium.in(1,i)),global.y(medium.in(1,i)),global.z(medium.in(1,i)), &
                                                    &           global.x(medium.in(2,i)),global.y(medium.in(2,i)),global.z(medium.in(1,i)), &
                                                    &           global.x(medium.in(3,i)),global.y(medium.in(3,i)),global.z(medium.in(1,i)), &
                                                    &           TriangleArea)
                                        
                                            TotOlfXYArea=TotOlfXYArea+TriangleArea
                                            if(medium.ZoneNum(i)==iZone) TotOlfZoneXYArea=TotOlfZoneXYArea+TriangleArea
                                        end do
                                        write(itmp,'(i5,2(a,f15.5),2a)') 0,",",TotOlfArea,",",TotOlfXYArea,",", 'OLFDomain'
                                        write(itmp,'(i5,2(a,f15.5),2a)') iZone,",",TotOlfZoneArea,",",TotOlfZoneXYArea,",", trim(Zonename)
                                    
                                    else
                                        TotOlfZoneArea=0.0d0
                                        TotOlfZoneXYArea=0.0d0
                                        do i=1,medium.NE
                                            if(medium.ZoneNum(i)==iZone) then 
                                                ! Calculate olf element true area
                                                call area_triangle(global.x(medium.in(1,i)),global.y(medium.in(1,i)),global.z(medium.in(1,i)), &
                                                        &           global.x(medium.in(2,i)),global.y(medium.in(2,i)),global.z(medium.in(2,i)), &
                                                        &           global.x(medium.in(3,i)),global.y(medium.in(3,i)),global.z(medium.in(3,i)), &
                                                        &           TriangleArea)
                                        
                                                TotOlfZoneArea=TotOlfZoneArea+TriangleArea 
                                        
                                                call area_triangle(global.x(medium.in(1,i)),global.y(medium.in(1,i)),global.z(medium.in(1,i)), &
                                                        &           global.x(medium.in(2,i)),global.y(medium.in(2,i)),global.z(medium.in(1,i)), &
                                                        &           global.x(medium.in(3,i)),global.y(medium.in(3,i)),global.z(medium.in(1,i)), &
                                                        &           TriangleArea)
                                        
                                                TotOlfZoneXYArea=TotOlfZoneXYArea+TriangleArea
                                            end if
                                        end do
                                        write(itmp,'(i5,2(a,f15.5),2a)') iZone,",",TotOlfZoneArea,",",TotOlfZoneXYArea,",", trim(Zonename)
                                    end if
                                end if
                                
                                    

                            case default ! read name from next line e.g. PM style
                                read(FNumEco,'(a)') line
                                read(line(2:),'(a)') ZoneName
                            end select
                
                            if(ichar(ZoneName(1:1))>=97 .and. ichar(ZoneName(1:1))<=122) then  ! a-z in column 1, convert to upper case
                                newIchar=ichar(ZoneName(1:1))-(97-65)
                                ZoneName(1:1)=char(newIchar)
                            end if
                            exit Zone
                        else
                            cycle Material
                        end if
                    end do Material
                    
                end do Zone
                
                TmpSTR=FileNumberString(iZone)
                if(iZone==1) then
                    labelset_M(medium.IDNum)='"'//trim(TmpSTR)//' '//trim(ZoneName)//'"'
                else
                    labelset_M(medium.IDNum)=trim(labelset_M(medium.IDNum))//',"'//trim(TmpSTR)//' '//trim(ZoneName)//'"'
                end if
            end if
          

                
        end do Domain_
        
        labelset_M(medium.IDNum)=trim(labelset_M(medium.IDNum))// C_NULL_CHAR
        
        continue

   end subroutine BuildCustomZoneNames

    !----------------------------------------------------------------------
    subroutine find_node(x1,y1,z1,node,global)
         
        implicit none
        type(HGSTecplotdomain) global

	    integer :: i
	    integer :: node
        real(dr) :: x1, y1, z1, dist_min, f1

        dist_min=1.0e20
	    do i=1,global.NN
		    f1=sqrt((x1-global.X(i))**2+((y1-global.Y(i)))**2+((z1-global.Z(i)))**2)
		    if(f1.lt.dist_min) then
			    node=i
			    dist_min=f1
		    end if
	    end do

	    write(ieco,'(a14,3f17.5)') 'Target x, y, z ',x1,y1,z1
	    write(ieco,'(a14,3f17.5)') 'Found x, y, z  ',global.X(node),Global.Y(node),Global.Z(node)
	    write(ieco,'(a14,3f17.5)') 'Delta x, y, z  ',Global.X(node)-x1,Global.Y(node)-y1,Global.Z(node)-z1

    end subroutine find_node
    !----------------------------------------------------------------------
    subroutine strip_comments(nin,nout)
         
        implicit none
        
        integer :: nin, nout, inc2
        character(256) ::  line_i
        logical :: file_exists
        
        

        read_line: do
            read(nin,'(a)',iostat=status) line_i
            if(status /= 0) exit read_line

            if(line_i(1:1) == '!' .or. len_trim(line_i) == 0) then  ! a comment or blank line_i, do nothing

            else if(line_i(1:7) == 'include') then ! include a file
                inquire(file=line_i(8:),exist=file_exists)
                if(.not. file_exists) then
                    call Msg( 'FILE ERROR:')
                    call Msg( line_i(8:))
                    stop
                else
                    call getunit(inc2)
                    open(inc2,file=line_i(8:),status='unknown',form='formatted')
                    read_include_line: do
                        read(inc2,'(a)',iostat=status) line_i
                        if(status /= 0) exit read_include_line

                        if(line_i(1:1) == '!' .or. len_trim(line_i) == 0) then  ! a comment or blank line_i, do nothing

                        else ! instruction or data, write to nout
                            write(nout,'(a)') adjustl(line_i)
                        end if

                    end do read_include_line
                    call freeunit(inc2)
                end if


            else ! instruction or data, write to nout
                write(nout,'(a)') adjustl(line_i)
            end if

        end do read_line

        rewind(nout)

    end subroutine strip_comments
    !----------------------------------------------------------------------
    subroutine read_nval(llabel, nval)
         
        implicit none
        integer nval
        character*(*) llabel

        read(itmp,*,iostat=status) nval
	    if(status /= 0) then
		    call Msg( 'Debug control - bad value:')
		    call Msg( llabel)
		    call Msg( ' ')
		    return
	    end if
        write(ieco,*) llabel
        write(ieco,*) 'Set to ',nval
    end subroutine read_nval
    
    !----------------------------------------------------------------------
    !  Rob: This is the original Tecplot example.
    subroutine Tecplot_ReadWrite_szplt(FnumMUT)
    !
    !  Read a .szplt file provided as the first command-line argument,
    !  and write a new .szplt file to the name provided as the second
    !  command-line argument.
    !


    ! Each tec function returns zero for success, non-zero for
    ! failure. We ignore these return codes for the sake of brevity.
        implicit none

        ! TG
        integer :: FnumMUT
        integer :: i, j


        !!!! Retrieve input and output file names
        !!!lastArgNum = iargc()
        !!!if (lastArgNum /= 2) then
        !!!    call getarg(0, programName)
        !!!  write(0,fmt='(a,a,a)') &
        !!!    "Usage: ", trim(programName), " infilename outfilename"
        !!!  stop
        !!!end if
        !!!
        !!!call getarg(1, inputFileName)
        !!!
        read(FnumMUT,'(a)') inputFileName
        inputFileName = trim(inputFileName) // C_NULL_CHAR

        !!!call getarg(2, outputFileName)
        read(FnumMUT,'(a)') outputFileName
        outputFileName = trim(outputFileName) // C_NULL_CHAR

        ! Open the input file for reading
        i = tecFileReaderOpen(inputFileName, inputFileHandle)

        ! Read info about the data set
        i = tecDataSetGetTitle(inputFileHandle, stringCPtr)
        call copyCharArrayToString(stringCPtr, &
            tecStringLength(stringCPtr), dataSetTitle)
        call tecStringFree(stringCPtr)
        i = tecDataSetGetNumVars(inputFileHandle, numVars)

        strLen = 0
        do var = 1, numVars
            i = tecVarGetName(inputFileHandle, var, stringCPtr)
            nameLen = tecStringLength(stringCPtr)
            call c_f_pointer(stringCPtr, stringPtr, [nameLen])
            if (var .gt. 1) then
                strLen = strLen + 1
                varNames(strLen : strLen) = ','
            end if
            do j = 1, nameLen
                varNames(strLen + j : strLen + j) = stringPtr(j)
            end do
            strLen = strLen + nameLen
            call tecStringFree(stringCPtr)
        end do
        varNames(strLen + 1 : strlen + 1) = C_NULL_CHAR

        i = tecFileGetType(inputFileHandle, fileType)
        i = tecDataSetGetNumZones(inputFileHandle, numZones)

        ! Open the output file
        i = tecFileWriterOpen(outputFileName, dataSetTitle, varNames, &
            fileFormat, fileType, 1, C_NULL_PTR, outputFileHandle)
        i = tecFileSetDiagnosticsLevel(outputFileHandle, outputDebugInfo)

        ! Zones
        do inputZone = 1, numZones
            i = tecZoneGetType(inputFileHandle, inputZone, zoneType)
            if (zoneType == 6 .or. zoneType == 7) &
                stop "Unsupported inputZone type."

            ! Retrieve info about the inputZone
            i = tecZoneGetTitle(inputFileHandle, inputZone, stringCPtr)
            call copyCharArrayToString(stringCPtr, &
                tecStringLength(stringCPtr), zoneTitle)
            call tecStringFree(stringCPtr)

            i = tecZoneGetIJK(inputFileHandle, inputZone, &
                iMax, jMax, kMax)

            allocate(varTypes(numVars))
            allocate(passiveVarList(numVars))
            allocate(valueLocation(numVars))
            allocate(shareVarFromZone(numVars))
            do var = 1, numVars
                i = tecZoneVarGetType(inputFileHandle, inputZone, &
                    var, varTypes(var))
                i = tecZoneVarIsPassive(inputFileHandle, inputZone, &
                    var, passiveVarList(var))
                i = tecZoneVarGetValueLocation(inputFileHandle, inputZone, &
                    var, valueLocation(var))
                i = tecZoneVarGetSharedZone(inputFileHandle, inputZone, &
                    var, shareVarFromZone(var))
            end do

            i = tecZoneConnectivityGetSharedZone(inputFileHandle, &
                inputZone, shareConnectivityFromZone)
            i = tecZoneFaceNbrGetMode(inputFileHandle, inputZone, &
                faceNeighborMode)
            if (faceNeighborMode > 4) faceNeighborMode = 1
            i = tecZoneFaceNbrGetNumConnections(inputfileHandle, &
                inputZone, numFaceConnections)

            if (zoneType == 0) then
                i = tecZoneCreateIJK(outputFileHandle, zoneTitle, &
                    iMax, jMax, kMax, varTypes, shareVarFromZone, &
                    valueLocation, passiveVarList, &
                    shareConnectivityFromZone, numFaceConnections, &
                    faceNeighborMode, outputZone)
            else
                i = tecZoneCreateFE(outputFileHandle, zoneTitle, zoneType, &
                    iMax, jMax, varTypes, shareVarFromZone, &
                    valueLocation, passiveVarList, &
                    shareConnectivityFromZone, numFaceConnections, &
                    faceNeighborMode, outputZone)
            end if

            i = tecZoneGetSolutionTime(inputFileHandle, inputZone, &
                solutionTime)
            i = tecZoneGetStrandID(inputFileHandle, inputZone, strandID)
            if (solutionTime /= 0.0 .or. strandID /= 0) &
                i = tecZoneSetUnsteadyOptions(outputFileHandle, &
                    outputZone, solutionTime, strandID)

            i = tecZoneGetParentZone(inputFileHandle, inputZone, &
                parentZone)
            if (parentZone /= 0) &
                i = tecZoneSetParentZone(inputFileHandle, outputZone, &
                    parentZone)

            ! Read and write inputZone data
            do var = 1, numVars
                if (passiveVarList(var) == 0 .and. &
                      shareVarFromZone(var) == 0) then
                    i = tecZoneVarGetNumValues(inputFileHandle, &
                              inputZone, var, numValues)
                    select case (varTypes(var))
                    case (1) ! float
                        allocate(floatValues(numValues))
                        i = tecZoneVarGetFloatValues(inputFileHandle, &
                            inputZone, var, 1_c_int64_t, numValues, &
                            floatValues)
                        i = tecZoneVarWriteFloatValues(outputFileHandle, &
                            outputZone, var, 0, numValues, floatValues)
                        deallocate(floatValues)
                    case (2) ! double
                        allocate(doubleValues(numValues))
                        i = tecZoneVarGetDoubleValues(inputFileHandle, &
                            inputZone, var, 1_c_int64_t, numValues, &
                            doubleValues)
                        i = tecZoneVarWriteDoubleValues(outputFileHandle, &
                            outputZone, var, 0, numValues, doubleValues)
                        deallocate(doubleValues)
                    case (3) ! int32_t
                        allocate(int32Values(numValues))
                        i = tecZoneVarGetInt32Values(inputFileHandle, &
                            inputZone, var, 1_c_int64_t, numValues, &
                            int32Values)
                        i = tecZoneVarWriteInt32Values(outputFileHandle, &
                            outputZone, var, 0, numValues, int32Values)
                        deallocate(int32Values)
                    case (4) ! int16_t
                        allocate(int16Values(numValues))
                        i = tecZoneVarGetInt16Values(inputFileHandle, &
                            inputZone, var, 1_c_int64_t, numValues, &
                            int16Values)
                        i = tecZoneVarWriteInt16Values(outputFileHandle, &
                            outputZone, var, 0, numValues, int16Values)
                        deallocate(int16Values)
                    case (5) ! uint8_t
                        allocate(int8Values(numValues))
                        i = tecZoneVarGetUInt8Values(inputFileHandle, &
                            inputZone, var, 1_c_int64_t, numValues, &
                            int8Values)
                        i = tecZoneVarWriteUInt8Values(outputFileHandle, &
                            outputZone, var, 0, numValues, int8Values)
                        deallocate(int8Values)
                    endselect
                end if
            end do

            deallocate(varTypes)
            deallocate(passiveVarList)
            deallocate(valueLocation)
            deallocate(shareVarFromZone)

            ! Write zone face neighbors, if any
            if (numFaceConnections > 0) then
                i = tecZoneFaceNbrGetNumValues(inputFileHandle, inputZone, &
                    numFaceValues)
                i = tecZoneFaceNbrsAre64Bit(inputFileHandle, inputZone, &
                    is64Bit)
                if (is64Bit == 1) then
                    allocate(faceConnections64(numFaceValues))
                    i = tecZoneFaceNbrGetConnections64(inputFileHandle, &
                        inputZone, faceConnections64)
                    i = tecZoneFaceNbrWriteConnections64(outputFileHandle, &
                        outputZone, faceConnections64)
                    deallocate(faceConnections64)
                else
                    allocate(faceConnections32(numFaceValues))
                    i = tecZoneFaceNbrGetConnections(inputFileHandle, &
                        inputZone, faceConnections32)
                    i = tecZoneFaceNbrWriteConnections32(outputFileHandle, &
                        outputZone, faceConnections32)
                    deallocate(faceConnections32)
                end if
            end if

            ! Retrieve zone node map, if any, and send to the output file
            if (zoneType /= 0 .and. &
                shareConnectivityFromZone == 0) then
                i = tecZoneNodeMapGetNumValues(inputFileHandle, &
                    inputZone, jMax, numValues)
                i = tecZoneNodeMapIs64Bit(inputFileHandle, inputZone, &
                    is64Bit)
                if (is64Bit == 1) then
                    allocate(nodeMap64(numValues))
                    i = tecZoneNodeMapGet64(inputFileHandle, &
                        inputZone, 1_8, jMax, nodeMap64)
                    i = tecZoneNodeMapWrite64(outputFileHandle, &
                        outputZone, 0, 1, numValues, nodeMap64)
                    deallocate(nodeMap64)
                else
                    allocate(nodeMap32(numValues))
                    i = tecZoneNodeMapGet(inputFileHandle, &
                        inputZone, 1_8, jMax, nodeMap32)
                    i = tecZoneNodeMapWrite32(outputFileHandle, &
                        outputZone, 0, 1, numValues, nodeMap32)
                    deallocate(nodeMap32)
                end if
            end if

            ! Zone aux data
            i = tecZoneAuxDataGetNumItems(inputFileHandle, inputZone, &
                numItems)
            do whichItem = 1, numItems
                i = tecZoneAuxDataGetItem(inputFileHandle, &
                        inputZone, whichItem, nameCPtr, valueCPtr)
                call copyCharArrayToString(nameCPtr, &
                         tecStringLength(nameCPtr), name)
                call copyCharArrayToString(valueCPtr, &
                         tecStringLength(valueCPtr), val)
                i = tecZoneAddAuxData(outputFileHandle, outputZone, &
                    name, val)
                call tecStringFree(nameCPtr)
                call tecStringFree(valueCPtr)
            end do

        end do ! inputZone loop

        ! Custom label sets
        i = tecCustomLabelsGetNumSets(inputFileHandle, numSets)
        do j = 1, numSets
            i = tecCustomLabelsGetSet(inputFileHandle, j, stringCPtr) ! Labels returned as "\"Mon\",\"Tues\",\"Wed\""
            call copyCharArrayToString(stringCPtr, &
                 tecStringLength(stringCPtr), labelSet)
            i = tecCustomLabelsAddSet(outputFileHandle, labelSet)
            call tecStringFree(stringCPtr)
        end do

        ! Data set aux data
        i = tecDataSetAuxDataGetNumItems(inputFileHandle, numItems)
        do j = 1, numItems
            i = tecDataSetAuxDataGetItem(inputFileHandle, &
                    j, nameCPtr, valueCPtr)
            call copyCharArrayToString(nameCPtr, &
                     tecStringLength(nameCPtr), name)
            call copyCharArrayToString(valueCPtr, &
                     tecStringLength(valueCPtr), val)
            i = tecDataSetAddAuxData(outputFileHandle, name, val)
            call tecStringFree(nameCPtr)
            call tecStringFree(valueCPtr)
        end do

        ! Var aux data
        do var = 1, numVars
            i = tecVarAuxDataGetNumItems(inputFileHandle, var, numItems)
            do whichItem = 1, numItems
                i = tecVarAuxDataGetItem(inputFileHandle, &
                        var, whichItem, nameCPtr, valueCPtr)
                call copyCharArrayToString(nameCPtr, &
                         tecStringLength(nameCPtr), name)
                call copyCharArrayToString(valueCPtr, &
                         tecStringLength(valueCPtr), val)
                i = tecVarAddAuxData(outputFileHandle, var, name, val)
                call tecStringFree(nameCPtr);
                call tecStringFree(valueCPtr);
            end do
        end do

        ! Geometries
        i = tecGeomGetNumGeoms(inputFileHandle, numGeoms)
        do geom = 1, numGeoms
            i = tecGeomGetType(inputFileHandle, geom, geomType)
            i = tecGeomGetAnchorPos(inputFileHandle, geom, xtecplot, ytecplot, ztecplot)
            i = tecGeomGetCoordMode(inputFileHandle, geom, coordMode)

            select case (geomType)
            case (0, 5) ! GeomType_LineSegs, GeomType_LineSegs3D
                i = tecGeomLineGetSegmentCount(inputFileHandle, &
                    geom, numSegments)
                allocate(numSegPts(numSegments))
                arrayLength = 0;
                do segment = 1, numSegments
                    i = tecGeomLineSegmentGetPointCount(inputFileHandle, &
                        geom, segment, numSegPts(segment))
                    arrayLength = arrayLength + numSegPts(segment)
                end do
                allocate(xGeomData(arrayLength))
                allocate(yGeomData(arrayLength))
                allocate(zGeomData(arrayLength))
                arrayLength = 0
                do segment = 1, numSegments
                    do ind = 1, numSegPts(segment)
                        arrayLength = arrayLength + 1
                        i = tecGeomLineGetPoint(inputFileHandle, &
                            geom, segment, ind, geomX, geomY, geomZ)
                        xGeomData(arrayLength) = geomX
                        yGeomData(arrayLength) = geomY
                        zGeomData(arrayLength) = geomZ
                    end do
                end do
                if (geomType == 0) then
                    if (numSegments == 1) then
                        i = tecGeom2DLineSegmentsBegin(outputFileHandle, &
                            xtecplot, ytecplot, numSegPts(1), &
                            xGeomData, yGeomData, coordMode)
                    else
                        i = tecGeom2DMultiLineSegmentsBegin( &
                            outputFileHandle, xtecplot, ytecplot, numSegments, &
                            numSegPts, xGeomData, yGeomData, coordMode)
                    end if
                else
                    if (numSegments == 1) then
                        i = tecGeom3DLineSegmentsBegin(outputFileHandle, &
                            xtecplot, ytecplot, ztecplot, numSegPts(1), &
                            xGeomData, yGeomData, zGeomData)
                    else
                        i = tecGeom3DMultiLineSegmentsBegin( &
                            outputFileHandle, xtecplot, ytecplot, ztecplot, numSegments, &
                            numSegPts, xGeomData, yGeomData, zGeomData)
                    end if
                end if
                deallocate(xGeomData, yGeomData, zGeomData)
                deallocate(numSegPts)

                ! The default is no arrowheads
                i = tecGeomArrowheadGetAngle(inputFileHandle, geom, &
                    arrowheadAngle)
                i = tecGeomArrowheadGetAttach(inputFileHandle, &
                    geom, arrowheadAttachment)
                i = tecGeomArrowheadGetSize(inputFileHandle, geom, &
                    arrowheadSize)
                i = tecGeomArrowheadGetStyle(inputFileHandle, geom, &
                    arrowheadStyle)
                if (arrowheadAttachment /= 0) &
                    i = tecGeomArrowheadSetInfo(outputFileHandle, &
                        arrowheadAngle, arrowheadAttachment, &
                        arrowheadSize, arrowheadStyle)
            case (1) ! GeomType_Rectangle
                i = tecGeomRectangleGetSize(inputFileHandle, &
                    geom, width, height)
                i = tecGeomRectangleBegin(outputFileHandle, xtecplot, ytecplot, &
                    xtecplot + width, ytecplot + height, coordMode)
            case (2) ! GeomType_Square
                i = tecGeomSquareGetSize(inputFileHandle, geom, &
                    squareSize)
                i = tecGeomSquareBegin(outputFileHandle, xtecplot, ytecplot, &
                    squareSize, coordMode)
            case (3) ! GeomType_Circle
                i = tecGeomEllipseGetNumPoints(inputFileHandle, &
                    geom, numEllipsePoints)
                i = tecGeomCircleGetRadius(inputFileHandle, geom, radius)
                i = tecGeomCircleBegin(outputFileHandle, xtecplot, ytecplot, &
                    radius, coordMode)
                i = tecGeomEllipseSetNumPoints(outputFileHandle, &
                    numEllipsePoints)
            case (4) ! GeomType_Ellipse
                i = tecGeomEllipseGetNumPoints(inputFileHandle, &
                    geom, numEllipsePoints)
                i = tecGeomEllipseGetSize(inputFileHandle, &
                    geom, horizontalAxis, verticalAxis)
                i = tecGeomEllipseBegin(outputFileHandle, xtecplot, ytecplot, &
                    horizontalAxis, verticalAxis, coordMode)
                i = tecGeomEllipseSetNumPoints(outputFileHandle, &
                    numEllipsePoints)
            case default
                write(0,fmt='(a,a,a)') "Unsupported geometry type ", &
                    geomType, ". Skipping."
            end select

            i = tecGeomGetClipping(inputFileHandle, geom, clipping)
            i = tecGeomSetClipping(outputFileHandle, clipping)

            i = tecGeomIsFilled(inputFileHandle, geom, isFilled)
            if (isFilled == 1) then
                i = tecGeomGetFillColor(inputFileHandle, geom, fillColor)
                i = tecGeomFill(outputFileHandle, fillColor)
            end if

            i = tecGeomGetLinePattern(inputFileHandle, geom, linePattern)
            i = tecGeomGetPatternLength(inputFileHandle, geom, &
                patternLength)
            i = tecGeomGetLineThickness(inputFileHandle, geom, &
                lineThickness)
            i = tecGeomGetColor(inputFileHandle, geom, color)
            i = tecGeomSetLineInfo(outputFileHandle, linePattern, &
                patternLength, lineThickness, color)

            i = tecGeomIsAttached(inputFileHandle, geom, isAttached)
            if (isAttached == 1) then
                i = tecGeomGetZone(inputFileHandle, geom, attachZone)
                i = tecGeomAttachToZone(inputFileHandle, attachZone)
            end if

            i = tecGeomGetMacroFunctionCmd(inputFileHandle, geom, &
                stringCPtr)
            call copyCharArrayToString(stringCPtr, &
                 tecStringLength(stringCPtr), macroFunctionCmd)
            call tecStringFree(stringCPtr)
            i = tecGeomSetMacroFunctionCmd(outputFileHandle, &
                macroFunctionCmd)

            i = tecGeomGetScope(inputFileHandle, geom, scope)
            i = tecGeomSetScope(outputFileHandle, scope)

            ! Close the output geom
            i = tecGeomEnd(outputFileHandle)

        end do

        ! Texts
        i = tecTextGetNumTexts(inputFileHandle, numTexts)
        do text = 1, numTexts
            i = tecTextGetString(inputFileHandle, text, stringCPtr)
            call copyCharArrayToString(stringCPtr, &
                 tecStringLength(stringCPtr), textString)
            call tecStringFree(stringCPtr)
            i = tecTextGetAnchorPos(inputFileHandle, text, xtecplot, ytecplot, ztecplot)
            i = tecTextGetCoordMode(inputFileHandle, text, coordMode)
            i = tecTextGetHeight(inputFileHandle, text, height)
            i = tecTextGetSizeUnits(inputFileHandle, text, sizeUnits)

            ! Begin a new output text
            if (coordMode == 6) then ! 3D
                i = tecText3DBegin(outputFileHandle, textString, xtecplot, ytecplot, ztecplot, &
                    height, sizeUnits)
            else
                i = tecText2DBegin(outputFileHandle, textString, xtecplot, ytecplot, &
                    coordMode, height, sizeUnits)
            end if

            ! All of the below have sensible defaults, but we'll set everything here for completeness
            i = tecTextGetColor(inputFileHandle, text, color)
            i = tecTextSetColor(outputFileHandle, color)

            i = tecTextBoxGetType(inputFileHandle, text, boxType)
            if (boxType /= 0) then
                i = tecTextBoxGetColor(inputFileHandle, text, boxColor)
                i = tecTextBoxGetFillColor(inputFileHandle, text, &
                    boxFillColor)
                i = tecTextBoxGetLineThickness(inputFileHandle, &
                    text, boxLineThickness)
                i = tecTextBoxGetMargin(inputFileHandle, text, boxMargin)

                i = tecTextBoxSetInfo(outputFileHandle, boxType, boxColor, &
                    boxFillColor, boxLineThickness, boxMargin)
            end if

            i = tecTextGetAnchor(inputFileHandle, text, anchor)
            i = tecTextSetAnchor(outputFileHandle, anchor)

            i = tecTextIsAttached(inputFileHandle, text, isAttached)
            if (isAttached == 1) then
                i = tecTextGetZone(inputFileHandle, text, attachZone)
                i = tecTextAttachToZone(outputFileHandle, attachZone)
            end if

            i = tecTextGetAngle(inputFileHandle, text, angle)
            i = tecTextSetAngle(outputFileHandle, angle)

            i = tecTextGetClipping(inputFileHandle, text, clipping)
            i = tecTextSetClipping(outputFileHandle, clipping)

            i = tecTextGetScope(inputFileHandle, text, scope)
            i = tecTextSetScope(outputFileHandle, scope)

            i = tecTextGetTypeface(inputFileHandle, text, stringCPtr)
            call copyCharArrayToString(stringCPtr, &
                     tecStringLength(stringCPtr), typeface)
            call tecStringFree(stringCPtr)
            i = tecTextIsBold(inputFileHandle, text, isBold)
            i = tecTextIsItalic(inputFileHandle, text, isItalic)
            i = tecTextSetTypeface(outputFileHandle, typeface, &
                isBold, isItalic)

            i = tecTextGetLineSpacing(inputFileHandle, text, lineSpacing)
            i = tecTextSetLineSpacing(outputFileHandle, lineSpacing)

            i = tecTextGetMacroFunctionCmd(inputFileHandle, &
                    text, stringCPtr)
            call copyCharArrayToString(stringCPtr, &
                     tecStringLength(stringCPtr), macroFunctionCmd)
            call tecStringFree(stringCPtr)
            i = tecTextSetMacroFunctionCmd(outputFileHandle, &
                macroFunctionCmd)

            ! Close the output text
            i = tecTextEnd(outputFileHandle)

        end do

        ! Close old and new files
        i = tecFileWriterClose(outputFileHandle)
        i = tecFileReaderClose(inputFileHandle)

    end subroutine Tecplot_ReadWrite_szplt

    ! Convenience to extract a returned C string to a FOrtran string
    subroutine copyCharArrayToString(charArray, length, string)
        use iso_c_binding, only : C_NULL_CHAR, c_ptr, c_f_pointer
        implicit none
        type(c_ptr) :: charArray
        integer length
        character(*) string

        character, pointer :: charPointer(:)
        integer i

        call c_f_pointer(charArray, charPointer, [length])

        string = ' '
        do i = 1, length
            string(i:i) = charPointer(i)
        end do
        string(length+1:length+1) = C_NULL_CHAR
        return
    end subroutine copyCharArrayToString
    


end module Tecplot !
