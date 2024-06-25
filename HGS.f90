module faces

	!-----------------------------------------------------------------------------------------------------
	! 4-node Block elements

	integer, parameter :: n_block_faces_external=6
	integer, parameter :: n_block_4node_faces_internal=6
	integer, parameter :: n_block_3node_faces_external=24
	integer, parameter :: n_block_3node_faces_internal=32
	integer*4 faceid_block(4, n_block_faces_external + n_block_4node_faces_internal + n_block_3node_faces_external + n_block_3node_faces_internal)

    !rt 20080723 - reduced number of continuation lines for faceid_block (got an error when compiling on Unix)
	data faceid_block/	&

		! 6 4-node outer faces
		! bottom    top         front       back        left        right 
        1,2,3,4,    5,6,7,8,    1,2,6,5,    4,3,7,8,    1,5,8,4,    2,6,7,3,  &  

		! 6 4-node internal faces
        1,2,7,8,    3,4,5,6,    2,3,8,5,    4,1,6,7,    8,4,2,6,    1,5,7,3,  &  

		! 24 3-node outer faces
        1,2,3,0,    1,2,4,0,    1,3,4,0,	2,3,4,0,  &		! bottom     
        5,6,7,0,    5,6,8,0,    5,7,8,0,    6,7,8,0,  &     ! top 
		1,2,5,0,  	1,2,6,0,	1,6,5,0,  	2,6,5,0,  &  	! front     
        4,3,8,0,    4,3,7,0,    4,7,8,0,    3,7,8,0,  &     ! back
		1,4,5,0,    1,4,8,0,    1,8,5,0,    4,8,5,0,  &     ! left       
        2,3,6,0,    2,3,7,0,    2,7,6,0,    3,7,6,0,  &     ! right 
        
		! 32 internal faces 
		1,2,7,0, 	1,2,8,0,    1,3,5,0,    1,3,6,0,  &
		1,3,7,0,    1,3,8,0,    1,4,6,0,    1,4,7,0,  &   
        1,7,6,0,  	1,6,8,0,    1,7,5,0,    1,7,8,0,  &   
        2,3,5,0,  	2,3,8,0,    2,5,4,0,    2,6,4,0,  &
		2,6,8,0,    2,7,4,0,    2,7,5,0,    2,7,8,0,  &   
        2,8,4,0,  	2,8,5,0,    3,5,4,0,    3,6,4,0,  &   
        3,6,5,0,    3,7,5,0,    3,8,5,0,    3,6,8,0,  &   
        4,6,5,0,  	4,7,5,0,    4,7,6,0,    4,6,8,0/
 
 	integer :: block_facemask(n_block_faces_external + n_block_4node_faces_internal + n_block_3node_faces_external + n_block_3node_faces_internal)
	!                       bottom          top             front           back            left            right
	!                       1,2,3,4         5,6,7,8         1,2,6,5         4,3,7,8         1,5,8,4         2,6,7,3 
	data block_facemask /   B'00001111',    B'11110000',    B'00110011',    B'11001100',    B'10011001',    B'01100110', &      

	! 6 4-node internal faces
    !                       1,2,7,8         3,4,5,6         2,3,8,5         4,1,6,7         8,4,2,6         1,5,7,3  
	                        B'11000011',    B'00111100',    B'10010110',    B'01101001',    B'10101010',    B'01010101', &      

	! 24 3-node outer faces
	!  bottom               1,2,3,0   		1,2,4,0  	    1,3,4,0    	    2,3,4,0 
	                        B'00000111',    B'00001011',    B'00001101',    B'00001110',     &      
	!  top                  5,6,7,0         5,6,8,0         5,7,8,0         6,7,8,0 
	                        B'01110000',    B'10110000',    B'11010000',    B'11100000',     &      
	!  front                1,2,5,0		    1,2,6,0   	    1,6,5,0  	 	2,6,5,0 
	                        B'00010011',    B'00100011',    B'00110001',    B'00110010',     &      
	!  back                 4,3,8,0         4,3,7,0         4,7,8,0         3,7,8,0 
	                        B'10001100',    B'01001100',    B'11001000',    B'11000100',     &      
    !  left      		    1,4,5,0         1,4,8,0         1,8,5,0         4,8,5,0
	                        B'00110001',    B'10001001',    B'10010001',    B'10011000',     &      
    !  right                2,3,6,0         2,3,7,0         2,7,6,0         3,7,6,0  
	                        B'00100110',    B'01000110',    B'01100010',    B'01100100',     &      
        
	! 32 internal faces 
	!	                    1,2,7,0         1,2,8,0         1,3,5,0,        1,3,6,0
	                        B'01000011',    B'10000011',    B'00010101',    B'00100101',     &      
	!	                    1,3,7,0         1,3,8,0         1,4,6,0,        1,4,7,0   
	                        B'01000101',    B'10000101',    B'00101001',    B'01001001',     &      
    !	                    1,7,6,0         1,6,8,0         1,7,5,0, 	    1,7,8,0   
	                        B'01100001',    B'10100001',    B'01010001',    B'11000001',     &      
    !	                    2,3,5,0         2,3,8,0         2,5,4,0,        2,6,4,0
	                        B'00010110',    B'10000110',    B'00011010',    B'00101010',     &      
	!	                    2,6,8,0         2,7,4,0         2,7,5,0, 	    2,7,8,0   
	                        B'10100010',    B'01001010',    B'01010010',    B'11000010',     &      
    !	                    2,8,4,0         2,8,5,0         3,5,4,0, 	    3,6,4,0   
	                        B'10001010',    B'10010010',    B'00011100',    B'00101100',     &      
    !	                    3,6,5,0         3,7,5,0         3,8,5,0,        3,6,8,0   
	                        B'00110100',    B'01010100',    B'10010100',    B'10100100',     &      
    !	                    4,6,5,0         4,7,5,0         4,7,6,0,        4,6,8,0
	                        B'00111000',    B'01011000',    B'01101000',    B'10101000'/      
	
	
	
	
	!-----------------------------------------------------------------------------------------------------
	! 6-node Prism elements
	
	integer, parameter :: n_prism_faces_external=5
	integer, parameter :: n_prism_faces_internal=6
	integer, parameter :: n_prism_3node_faces_external=12

	integer faceid_prism(4,n_prism_faces_external+n_prism_faces_internal+n_prism_3node_faces_external)

	data faceid_prism/ &
		! 5 3-node, 4-node outer faces
		! bottom    top         s1          s2          s3 
		1,2,3,0,    4,5,6,0,    1,2,5,4,    1,3,6,4,    2,3,6,5,  &  

		! 6 3-node internal faces
		1,6,5,0,	2,6,4,0,  	3,4,5,0,  	4,2,3,0,  	5,3,1,0,    6,1,2,0,  & 

		! 12 3-node outer faces
        1,2,4,0,    2,5,1,0,    5,4,2,0,    4,1,5,0,  &  	! s1 
        1,3,4,0,    3,6,1,0,    6,4,3,0,    4,1,6,0,  &     ! s2       
        2,3,5,0,    3,6,2,0,    6,5,3,0,    5,2,6,0   /     ! s3 


 	integer :: prism_facemask(n_prism_faces_external+n_prism_faces_internal+n_prism_3node_faces_external)
	!	                    bottom          top             s1              s2              s3 
	!	                    1,2,3,0         4,5,6,0         1,2,5,4         1,3,6,4         2,3,6,5  
	data prism_facemask /   B'00000111',    B'00111000',    B'00011011',    B'00101101',    B'00110110',    &      

	! 6 3-node internal faces
    !                       1,6,5,0	        2,6,4,0  	    3,4,5,0  	    4,2,3,0  	    5,3,1,0         6,1,2,0,  & 
	                        B'00110001',    B'00101010',    B'00011100',    B'00001110',    B'00010101',    B'00100011', &      

	! 12 3-node outer faces
    !  s1                   1,2,4,0         2,5,1,0         5,4,2,0         4,1,5,0
	                        B'00001011',    B'00010011',    B'00011010',    B'00011001',  &      
    !  s2                   1,3,4,0         3,6,1,0         6,4,3,0         4,1,6,0 
	                        B'00001101',    B'00100101',    B'00101100',    B'00101001',  &      
    !  s3                   2,3,5,0         3,6,2,0         6,5,3,0         5,2,6,0
	                        B'00010110',    B'00100110',    B'00110100',    B'00110010' /      
	                        

	!-----------------------------------------------------------------------------------------------------
	! 4-node tetrahedra elements

	integer, parameter :: n_tetra_faces=4

	integer*4 faceid_tetra(3,n_tetra_faces)

   	!DB dec-06 Numbered according to LaGriT local facets, 
   	data faceid_tetra/ &                       
		! 4 3-node outer faces
        ! face1     face2       face3       face4 
		2,3,4,      1,4,3,      1,2,4,      1,3,2/       
		
	integer(2) :: tetra_facemask(n_tetra_faces)
	data tetra_facemask / &
	        ! face1     face2       face3       face4 
		    !2,3,4      1,4,3       1,2,4       1,3,2       
	        B'1110',    B'1101',    B'1011',    B'0111'/	

		        


    ! temporay storage of element and face number lists
    integer :: nflist_cur=0
    integer, allocatable :: elist(:),flist(:)
end module faces
module HGS !
    use GeneralRoutines
    use fem
    use faces
    use raster
    implicit none
    
    character(256) :: HGS_CMD  
    character(60) :: HGS_GetMeshComponents_CMD='read components'
    !character(60) :: HGS_WriteComponents_CMD='write components'
    !character(60) :: HGS_ReadNodalOutput_CMD='read nodal output file'
    !character(60) :: HGS_WriteNodalOutput_CMD='write nodal output file'
    !character(60) :: HGS_ReadNodalOutput4_CMD='read nodal output file, single precision'
    !character(60) :: HGS_ReadNodalAscii_CMD='read nodal ascii file'
    !character(60) :: HGS_ReadNodelist_CMD='read node list'
    !character(60) :: HGS_ReadElementlist_CMD='read element list'
    !character(60) :: HGS_ReadElemK_CMD='read elemental k file'
    !character(60) :: HGS_ReadElemPor_CMD='read elemental porosity file'
    !character(60) :: HGS_ReadElemStor_CMD='read elemental specific storage file'
    !character(60) :: HGS_ReadElemRech_CMD='read elemental recharge file'
    !character(60) :: HGS_ReadWatBalFile_CMD='read water balance file'
    !character(60) :: HGS_WriteNodeSheetsToSurfer_CMD='write node sheets as 2d surfer files'
    !character(60) :: HGS_WriteNodeListsByTopSheet_CMD='write node lists by top sheet'
    !character(60) :: HGS_WriteNodeSheetsGeometryToSurfer_CMD='write node sheets geometry as 2d surfer files'
    !character(60) :: HGS_WriteElementCentroids_CMD='write element centroids'
    !character(60) :: HGS_ExtractDataFromLstFile_CMD='extract data from lst file'
    !character(60) :: HGS_ExtractMnthlyFlowWatBalFile_CMD='extract monthly flows from water balance file'
    !character(60) :: HGS_IntegrateFlowWatBalFile_CMD='integrate flows from water balance file'
    !character(60) :: HGS_WaterYearAccumulation_CMD='water year accumulation'
    !
    !character(60) :: HGS_TeckRatesWatBalFile_CMD='teck: eta rates from water balance file'
    !character(60) :: HGS_ConvertObsWellToPoint_CMD='convert obs well to point'
    !character(60) :: HGS_InterpolateHydrographs_CMD='interpolate hydrographs to common timestep'
    !character(60) :: HGS_ConcatenateTimeSeries_CMD='concatenate time series'
    character(60) :: HGS_ToTecplot_CMD='hgs to tecplot'
    !character(60) :: HGS_ZoneBudget_CMD='zone budget'
    !character(60) :: HGS_AdjustTopGbNprop_CMD='adjust top elevation from gb nprop file'
    !character(60) :: HGS_ViscosityVsTemp_CMD='viscosity vs temperature'
    !character(60) :: HGS_FluidProps_CMD='fluid properties'
    !character(60) :: HGS_AdjustZFromChosenNodesAndSheetNumber_CMD='adjust z from chosen nodes and sheet number'
    !character(60) :: HGS_AdjustSurfaceFromGBNprop_CMD='adjust surface from gb nprop'
    !character(60) :: HGS_ReadGBNprop_CMD='read gb nprop to hgs structure'
    !character(60) :: HGS_ReadGBNchos_CMD='read gb nchos to hgs structure'
    !character(60) :: HGS_CompareKFields_CMD='compare k fields'
    !character(60) :: HGS_ComparePorFields_CMD='compare porosity fields'
    !character(60) :: HGS_CompareStorFields_CMD='compare specific storage fields'
    !character(60) :: HGS_CompareRechFields_CMD='compare recharge fields'
    !character(60) :: HGS_WriteNodalOutputToFeflowDacFormat_CMD='write nodal output to feflow dac file format'
    !character(60) :: HGS_KvsDepthCrossSection_CMD='k vs depth cross section'
    !character(60) :: HGS_KvsDepthCrossSectionV2_CMD='k vs depth cross section v2'
    !character(60) :: HGS_RechargeCrossSection_CMD='recharge cross section'
    !character(60) :: HGS_BC_unsat_functions_gen_tables_CMD='generate brooks-cory soil retention tables from parameters'
    !character(60) :: HGS_VG_unsat_functions_gen_tables_CMD='generate van genuchten soil retention tables from parameters'
    !character(60) :: HGS_ReplaceDTW_With_FeflowSliceNumber_CMD='replace depthtogwt with feflow slice number'
    !character(60) :: HGS_BuildCustomZoneNames_CMD='build custom zone names'
    !character(60) :: HGS_ElementalKZoneTable_CMD='elemental k zone table'
    !character(60) :: HGS_PostGrokAnalysis_CMD='post grok analysis'
    !character(60) :: HGS_GatherObsWellFlowFiles_CMD='gather observation well flow files'
    !character(60) :: HGS_NodalSumByNodeList_CMD='nodal sum by node list'
    !character(60) :: HGS_FracFromNodeList_CMD = 'build hgs fractures from node list'
    !
    !! Workflows
    !character(60) :: HGS_CSVTableToMprops_CMD='convert csv table to mprops file'
    !character(60) :: HGS_CSVTableToObsPts_CMD='convert csv table to observation points files'
    !character(60) :: HGS_CSVFileToTransCalibPlots_CMD="csv file to transient calibration plots"
    !character(60) :: HGS_CalibrationSnapshot_CMD='calibration snapshot'
    !
    !! Superceded but kept for backwards compatibility
    !character(60) :: HGS_AddFeflowSliceNumber_CMD='add feflow slice number'  ! now handled by Tecplot_hsplot2012_CMD="hsplot 2012"
    !
    !! character(60) :: HGS_TecIOTest_CMD='tecio test'    
    !!!!character(60) :: HGS_HGSDirToTecplot_CMD='hgs directory to tecplot'    
    
    character(60) :: HGS_End_CMD=	'end hgs'
    
    type HGSProject
        
        type(femesh) mesh
        
        character(128) :: Name
        integer :: LengthName
        logical :: Exists=.false.
	    integer :: Unit
	    integer	:: CplScheme
        
        character(128) :: Prefix

        
        logical :: blockel


        logical,allocatable :: nchosen(:)
        
	    real(dr), allocatable :: Head(:)    
	    real, allocatable :: Sat(:)    
	    real(dr), allocatable :: ElemKx(:)                   
	    real(dr), allocatable :: ElemKy(:)                   
	    real(dr), allocatable :: ElemKz(:)                   
	    real, allocatable :: ElemPor(:)					
	    real(dr), allocatable :: ElemStor(:)					
	    real(dr), allocatable :: ElemTort(:)					
	    real(dr), allocatable :: ElemRech(:)  ! top layer i.e. of size ne2d 					  

	    real, allocatable :: nprop(:)					
	    logical, allocatable :: nchos(:)					

        integer	:: nzones_prop = 0
        

        integer :: nx, ny, nz, nsptot, nsheet
        integer :: ne2d, nb2d, nn2d

        logical,allocatable :: echosen(:)
        integer*4,allocatable  :: iprop(:)
        
        ! faces
        logical :: faces_calculated
        integer :: nface  = 0
	    integer :: nf_cur
        integer,allocatable :: face_el(:,:) ! nface,2)
        integer,allocatable :: face_olf_el(:) ! nface)
        integer,allocatable :: face_node(:,:) ! nface,4)
	    logical :: internal_faces=.false.
	    logical				:: requires_face_info = .false. 
        integer, allocatable :: nf_nd(:)        ! number of faces/node in list
	    integer, allocatable :: face_index(:,:) ! face_index(i,nf_nd(i)) is list of faces for node i

        ! segments
        logical :: segments_calculated    
        integer				:: nseg2d  = 0
        integer				:: nseg  = 0
	    integer :: ns_cur
        integer,allocatable :: seg_node(:,:) ! nseg,2)
        integer,allocatable :: nscon(:) ! maxnn)
        integer,allocatable :: scon(:,:) ! maxnn,max seg connections/node)
        
        ! bit flags
        integer,allocatable :: node_is(:)
        integer,allocatable :: elem_is(:)
        integer,allocatable :: face_is(:)
        
        ! fractures
        integer	:: nzones_fprop = 0
	    integer :: frac_scheme = 0
        integer :: nnfrac = 0
        real(dr), allocatable :: ap(:)
        integer, allocatable :: ifrac_id_elem(:)
        integer, allocatable :: ifrac_3d_elem_map(:,:)	! Fracture flow , 3D elements corresponding to 2D fracture elements
        integer, ALLOCATABLE :: link_frac2pm(:)			! maps frac unknown index to pm (or 3D grid) nodal number - dimension is nnfrac
        integer, ALLOCATABLE :: link_pm2frac(:)			! maps pm (or 3D grid) nodal number to frac unknown index to  - dimension is nn
													! (default value is 0 if no frac unknown corresponds to a given subsurface node)

        ! overland
        integer	:: nzones_oprop = 0
	    integer :: olf_scheme = 0
        integer :: nnolf = 0
        integer, allocatable :: iOlf_id_elem(:)
        integer, allocatable :: iOlf_3d_elem_map(:)	! Overland flow , 3D elements corresponding to 2D overland elements
        integer, ALLOCATABLE :: link_Olf2pm(:)			! maps Olf unknown index to pm (or 3D grid) nodal number - dimension is nnOlf
        integer, ALLOCATABLE :: link_pm2Olf(:)			! maps pm (or 3D grid) nodal number to Olf unknown index to  - dimension is nn
													! (default value is 0 if no olf unknown corresponds to a given subsurface node)


         ! hgs related arrays
        real(dr) :: IheadMax 
        real(dr) :: IheadMin
        real(dr), allocatable :: InitialHead(:)   
        real(dr), allocatable :: ConstantHead(:)  
        real(dr), allocatable :: ConstrainedHead(:)  
        real(dr), allocatable :: ConstrainedHead2(:)  
        real(dr), allocatable :: NodalFlux(:)   
        real(dr), allocatable :: DistributedFlux(:)   
        logical, allocatable :: SeepNodeFlag(:)   
        integer, allocatable :: ncountARRAYIhead(:)
        integer, allocatable :: ncountARRAY(:)


        integer, allocatable :: bc_kind_array(:)
        
        real(dr) :: p32

        character(80) :: message
        
        integer :: nNodeList=0
        integer, allocatable :: NodeListSize(:)
        integer, allocatable :: NodeList(:,:)

        integer :: nElementList=0
        integer, allocatable :: ElementListSize(:)
        integer, allocatable :: ElementList(:,:)
        
        ! soil retention properties
        character(100) :: prop_name
        real(dr) :: swr
        real(dr) :: alpha
        real(dr) :: beta
        real(dr) :: gamma
        real(dr) :: expn
	    logical :: expn_user  
        real(dr) :: pore_connect
        real(dr) :: aentry
        real(dr) :: krwmin
        real(dr) :: tab_pmin
        real(dr) :: tab_s_k_slp_max
        real(dr) :: tab_smooth
        
        character(MAXLBL) :: PMZoneName(100)
        character(MAXLBL) :: ZoneName(100)
        
        character(MAXSTRING) :: TitleLine
        character(MAXSTRING) :: VarLine
        character(MAXSTRING) :: ZoneLine
        integer :: nVars
        integer :: nLines
        character(100), allocatable :: VarName(:)        ! nvars
        real(dr), allocatable :: SimTime(:) ! nlines
        real(dr), allocatable :: Rate(:,:)  ! nlines,nvars
   
    end type HGSProject
    

    ! bit flags    
    integer :: active=1
    integer :: contributing=2
    integer, parameter  :: a_frac = 9
    
    logical :: tetramesh
    logical :: gal_mode
    logical :: do_write_face_seg 
    
    character(MAXLBL) :: GrokPfx
    integer :: LGrokPfx   
    
    logical :: DoSurfaceFlowGrok
    logical :: DoWellGrok

    character(MAXLBL) :: HGSPfx
    integer :: LHGSPfx   

    character(MAXLBL) :: GbPfx
    integer :: LGbPfx   


    integer :: FnumGB

    integer :: FNumCoor
    integer :: FNumElem
    integer :: FNumActive
    character(MAXLBL) :: FNameActive
    integer :: FNumContrib
    character(MAXLBL) :: FNameContrib
    integer :: FnumIhead
    integer :: FnumElemSet        
    integer :: FnumNodeSet        
    integer :: FnumNodeList    ! for distributed flux         
    integer :: FnumNodeSetValues        
    integer :: FnumFaceSet
    integer :: FnumInactive
    integer :: FnumSeepNodes
    integer :: FnumElemKTensor
    integer :: FnumElemPor
    integer :: FnumElemStor
    

    integer :: FnumVariableElemK        
    real(dr),allocatable :: Kxx(:)
    real(dr),allocatable :: Kyy(:)
    real(dr),allocatable :: Kzz(:)
    logical :: k_variable=.false.

    integer :: FnumVariablePor        
    real(dr),allocatable :: por(:)
    logical :: por_variable=.false.

    integer :: FnumVariableStor        
    real(dr),allocatable :: stor(:)
    logical :: stor_variable=.false.

    !integer, allocatable :: elist(:)  ! list of elements for face set
    
    logical :: SeepageBC  ! need to run HGS in transient mode
    real(dr) :: InitialTimestep=1.e-4
    real(dr) :: FinalTime=1.e8
    real(dr) :: HeadControl=5.

    real(dr) :: viscosity
    real(dr) :: Temp, Temp1, Temp2

    !real(dr) :: grav, rho, visc
    real(dr) :: cfactor
    
    character(512) :: VarList
    integer :: Nvar
	integer :: XNvar
	integer :: YNvar
	integer :: ZNvar
	integer :: ZoneNvar
	integer :: HeadNvar

    ! Tecio variables
    character*1 :: NULLCHR
    Integer*4 :: VIsDouble
    Integer*4 :: FileType
    Integer*4 :: FileFormat
    Integer*4 :: Debug
    Real*8    :: SolTime
    Integer*4 :: ZoneType,StrandID,ParentZn,IsBlock
    Integer*4 :: ICellMax,JCellMax,KCellMax,NFConns,FNMode,ShrConn
    POINTER   (NullPtr,Null)
    Integer*4 :: Null(*)
    integer   :: Imax, Kmax, Jmax
    Integer*4 :: isDouble


    contains
    
   !------------------------------------------------------------------------
   subroutine ProcessHGS(FnumTG, hgs) !--- Process HGS instructions for this data structure  hgs
        implicit none
    
        integer :: FnumTG
    
        type (HGSProject) hgs  
        
        do
            read(FnumTG,'(a)',iostat=status,end=10) HGS_CMD
            call Msg('!-----------------------')
            call Msg('HGS:'//HGS_CMD)
            
            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.HGS'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
            end if
            
            if(index(HGS_CMD, HGS_GetMeshComponents_CMD)  /= 0) then
                call HGS_GetMeshComponents(FnumTG, hgs)
                
            !else if(index(HGS_CMD, HGS_WriteComponents_CMD)  /= 0) then
            !    call HGS_WriteComponents(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadNodalOutput_CMD)  /= 0) then
            !    call HGS_ReadNodalOutput(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_WriteNodalOutput_CMD)  /= 0) then
            !    call HGS_WriteNodalOutput(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadNodalOutput4_CMD)  /= 0) then
            !    call HGS_ReadNodalOutput4(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadNodalAscii_CMD)  /= 0) then
            !    call HGS_ReadNodalAscii(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadNodelist_CMD)  /= 0) then
            !    call HGS_ReadNodelist(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadElementlist_CMD)  /= 0) then
            !    call HGS_ReadElementlist(FnumTG, hgs)
            !    
            !else if(index(HGS_CMD, HGS_ReadElemK_CMD)  /= 0) then
            !    call HGS_ReadElemK(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadElemPor_CMD)  /= 0) then
            !    call HGS_ReadElemPor(FnumTG, hgs)
            !else if(index(HGS_CMD, HGS_ReadElemStor_CMD)  /= 0) then
            !    call HGS_ReadElemStor(FnumTG, hgs)
            !else if(index(HGS_CMD, HGS_ReadElemRech_CMD)  /= 0) then
            !    call HGS_ReadElemRech(FnumTG, hgs)
            !else if(index(HGS_CMD, HGS_ReadWatBalFile_CMD)  /= 0) then
            !    call HGS_ReadWatBalFile(FnumTG, hgs)
            !else if(index(HGS_CMD, HGS_CompareKFields_CMD)  /= 0) then
            !    call HGS_CompareKFields(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ComparePorFields_CMD)  /= 0) then
            !    call HGS_ComparePorFields(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_CompareRechFields_CMD)  /= 0) then
            !    call HGS_CompareRechFields(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_CompareStorFields_CMD)  /= 0) then
            !    call HGS_CompareStorFields(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_WriteNodeSheetsToSurfer_CMD)  /= 0) then
            !    call HGS_WriteNodeSheetsToSurfer(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_WriteNodeListsByTopSheet_CMD)  /= 0) then
            !    call HGS_WriteNodeListsByTopSheet(hgs)
            !
            !else if(index(HGS_CMD, HGS_WriteNodeSheetsGeometryToSurfer_CMD)  /= 0) then
            !    call HGS_WriteNodeSheetsGeometryToSurfer(hgs)
            !
            !else if(index(HGS_CMD, HGS_WriteNodalOutputToFeflowDacFormat_CMD)  /= 0) then
            !    call HGS_WriteNodalOutputToFeflowDacFormat(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_AdjustZFromChosenNodesAndSheetNumber_CMD)  /= 0) then
            !    call HGS_AdjustZFromChosenNodesAndSheetNumber(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_AdjustSurfaceFromGBNprop_CMD)  /= 0) then
            !    call HGS_AdjustSurfaceFromGBNprop(FnumTG, hgs)
            !
            !    
            !else if(index(HGS_CMD, HGS_WriteElementCentroids_CMD)  /= 0) then
            !    call HGS_WriteElementCentroids(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ExtractDataFromLstFile_CMD)  /= 0) then
            !    call HGS_ExtractDataFromLstFile(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ExtractMnthlyFlowWatBalFile_CMD)  /= 0) then
            !    call HGS_ExtractMnthlyFlowWatBalFile(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_WaterYearAccumulation_CMD)  /= 0) then
            !    call HGS_WaterYearAccumulation(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_TeckRatesWatBalFile_CMD)  /= 0) then
            !    call HGS_TeckRatesWatBalFile(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_IntegrateFlowWatBalFile_CMD)  /= 0) then
            !    call HGS_IntegrateFlowWatBalFile(FnumTG)
            !    
            !else if(index(HGS_CMD, HGS_ConvertObsWellToPoint_CMD)  /= 0) then
            !    call HGS_ConvertObsWellToPoint(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_InterpolateHydrographs_CMD)  /= 0) then
            !    call HGS_InterpolateHydrographs(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_ConcatenateTimeSeries_CMD)  /= 0) then
            !    call HGS_ConcatenateTimeSeries(FnumTG)
            !
            !
            else if(index(HGS_CMD, HGS_ToTecplot_CMD)  /= 0) then
                call HGS_ToTecplot(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ZoneBudget_CMD)  /= 0) then
            !    call HGS_Zonebudget2(FnumTG, hgs)
            !
            !else if(index(HGS_CMD, HGS_ViscosityVsTemp_CMD)  /= 0) then
            !    read(FnumTG,*) Temp1, Temp2
	           ! call openascii(itmp,"visc_vs_temp.dat")
            !    write(itmp,'(a)') 'variables="Temp [C]","Viscosity"'
            !    write(itmp,'(a)') 'zone t="HGS Viscosity vs T function"'
            !    do Temp=Temp1,Temp2
            !        viscosity=fluid_viscosity_temperature(Temp)
            !        write(itmp,*) Temp,viscosity
            !    end do
            !    call freeunit(itmp)
            ! 
            !else if(index(HGS_CMD, HGS_FluidProps_CMD)  /= 0) then
            !    call HGS_ReadFluidProps(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_AdjustTopGbNprop_CMD)  /= 0) then
            !    call HGS_AdjustTopGbNprop(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadGBNprop_CMD)  /= 0) then
            !    call HGS_ReadGBNprop(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_ReadGBNchos_CMD)  /= 0) then
            !    call HGS_ReadGBNchos(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_KvsDepthCrossSection_CMD)  /= 0) then
            !    call HGS_KvsDepthCrossSection(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_KvsDepthCrossSectionV2_CMD)  /= 0) then
            !    call HGS_KvsDepthCrossSectionV2(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_RechargeCrossSection_CMD)  /= 0) then
            !    call HGS_RechargeCrossSection(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_BuildCustomZoneNames_CMD)  /= 0) then
            !    call HGS_BuildCustomZoneNames(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_ElementalKZoneTable_CMD)  /= 0) then
            !    call HGS_ElementalKZoneTable(FnumTG,hgs)
            !
            !    
            !!else if(instruction .eq. HGS_VG_unsat_functions_gen_tables_CMD) then
            !!    call HGS_VG_unsat_functions_gen_tables(FnumTG,hgs)
            !!else if(instruction .eq. HGS_BC_unsat_functions_gen_tables_CMD) then
            !!    call HGS_BC_unsat_functions_gen_tables(FnumTG,hgs)
            !!
            !    
            !else if(index(HGS_CMD, HGS_ReplaceDTW_With_FeflowSliceNumber_CMD)  /= 0) then
            !    call HGS_ReplaceDTW_With_FeflowSliceNumber(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_AddFeflowSliceNumber_CMD)  /= 0) then
            !    call HGS_AddFeflowSliceNumber(FnumTG,hgs)
            !
            !else if(index(HGS_CMD, HGS_CSVTableToMprops_CMD)  /= 0) then
            !    call HGS_CSVTableToMprops(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_CSVTableToObsPts_CMD)  /= 0) then
            !    call HGS_CSVTableToObsPts(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_CSVFileToTransCalibPlots_CMD)  /= 0) then
            !    call HGS_CSVFileToTransCalibPlots(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_CalibrationSnapshot_CMD)  /= 0) then
            !    call HGS_CalibrationSnapshot(FnumTG, hgs)
            !    
            !else if(index(HGS_CMD, HGS_GatherObsWellFlowFiles_CMD)  /= 0) then
            !    call HGS_GatherObsWellFlowFiles(FnumTG)
            !
            !else if(index(HGS_CMD, HGS_FracFromNodeList_CMD)  /= 0) then
            !    call HGS_FracFromNodeList(FnumTG, hgs)
            !
            !
            !!else if(index(HGS_CMD, HGS_NodalSumByNodeList_CMD)  /= 0) then
            !!    call HGS_NodalSumByNodeList_CMD(FnumTG,hgs)
            !    
            !!else if(index(HGS_CMD, HGS_TecIOTest_CMD)  /= 0) then
            !!    call HGS_TecIOTest
            !
            !!!!else if(index(HGS_CMD, HGS_HGSDirToTecplot_CMD)  /= 0) then
            !!!!    call HGS_HGSDirToTecplot(FnumTG,hgs)

            else if(index(HGS_CMD, HGS_End_CMD)  /= 0) then
                exit
            
            else
                call ErrMsg('HGS?:'//HGS_CMD)
            end if
        end do

        10 continue        
   
    end subroutine ProcessHGS
    !------------------------------------------------------------------------

    subroutine HGS_GetMeshComponents(FnumTG, hgs)
        implicit none

        integer :: FnumTG
        type (HGSProject) hgs
        
        integer :: i, j
        logical :: FileExists
        character(MAXLBL) :: fname

     !   character(60) :: HGS_PorousMedia_CMD="porous media"
     !   character(60) :: HGS_Surface_CMD="surface"
     !   character(60) :: HGS_Fracture_CMD="fracture"
	    !character(60) :: HGS_face_segment_info_CMD=	'face/segment data'
     !   character(60) :: HGS_EndComponent_CMD=	'end components'

	    hgs.segments_calculated=.false.
	    hgs.faces_calculated=.false.

       ! read_GridComponent_instructions: do
       !     read(FnumTG,'(a)',iostat=status,end=10) HGS_CMD
       !     call Msg('!-----------------------')
       !     call Msg('HGS:'//HGS_CMD)
       !     
       !     if(status/=0) then
 		    !    write(ErrStr,'(a)') 'File: '//'?file??'
		     !   l1=len_trim(ErrStr)
		     !   write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    !call ErrMsg(ErrStr)
       !     end if
            
        FName=trim(hgs.prefix)//'o.coordinates_pm'
        inquire(file=FName,exist=FileExists)
        if(FileExists) then
            ! coordinate data
            call Msg('Reading HGS PM coordinate file: '//fname)
	        call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')
            read(itmp) hgs.mesh.nn
            allocate(hgs.mesh.x(hgs.mesh.nn),hgs.mesh.y(hgs.mesh.nn),hgs.mesh.z(hgs.mesh.nn), stat=ialloc)
            call AllocChk(ialloc,'Read 3d grid node arrays')
            hgs.mesh.x = 0 ! automatic initialization
            hgs.mesh.y = 0 ! automatic initialization
            hgs.mesh.z = 0 ! automatic initialization
            read(itmp) (hgs.mesh.x(i),hgs.mesh.y(i),hgs.mesh.z(i),i=1,hgs.mesh.nn)
            read(itmp) hgs.nx,hgs.ny,hgs.nz,hgs.nsptot
            hgs.nsheet=hgs.nz
	        read(itmp) hgs.ne2d  ! for ET stuff
	        read(itmp) tetramesh	!DB feb-07
            read(itmp) gal_mode	!DB may-07
	        read(itmp) do_write_face_seg
	        read(itmp) hgs.nb2d
	        call freeunit(itmp)
                
            allocate(hgs.node_is(hgs.mesh.nn))
            call AllocChk(ialloc,'node_is bit setting array')


            ! element data
            FName=trim(hgs.prefix)//'o.elements_pm'
            call msg('Reading HGS PM element file: '//fname)

	        call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')
            read(itmp) hgs.mesh.nln
            read(itmp) hgs.mesh.ne
            allocate(hgs.mesh.in(hgs.mesh.nln,hgs.mesh.ne),stat=ialloc)
            call AllocChk(ialloc,'Read 3d grid element arrays')
            hgs.mesh.in = 0 ! automatic initialization
            read(itmp) ((hgs.mesh.in(j,i),j=1,hgs.mesh.nln),i=1,hgs.mesh.ne)
            allocate(hgs.iprop(hgs.mesh.ne),stat=ialloc)
            call AllocChk(ialloc,'Read 3d grid element property array')
            hgs.iprop = 0 ! automatic initialization
            read(itmp) (hgs.iprop(i),i=1,hgs.mesh.ne)
	        call freeunit(itmp)
	        
            !read_grid_components_pm    = .true.

            !rt 20010619
            hgs.nn2d = hgs.mesh.nn/hgs.nz
            hgs.ne2d = hgs.mesh.ne/(hgs.nz-1)

            hgs.blockel=.true.
            if(hgs.mesh.nln==6 .or. hgs.mesh.nln==4) hgs.blockel=.false.


            ! compute nzones_prop
            hgs.nzones_prop=0
            do i=1,hgs.mesh.ne
                if(hgs.iprop(i) > hgs.nzones_prop) hgs.nzones_prop=hgs.iprop(i)
            end do
            !call user_size_check(hgs.nzones_prop,user_zones,user_zones_str)
                
            !call calc_3d_faces(hgs)
        end if

        FName=trim(hgs.prefix)//'o.coordinates_olf'
        inquire(file=FName,exist=FileExists)
        if(FileExists) then
            call Msg('Reading HGS OLF coordinate file: '//fname)
            call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')
   	        read(itmp) hgs.nnolf
	        read(itmp) hgs.olf_scheme
            allocate(hgs.link_Olf2pm(hgs.nnOlf), &
                hgs.link_pm2Olf(hgs.mesh.nn),  stat=ialloc)
            call AllocChk(ialloc,'Olf node arrays ')
            hgs.link_Olf2pm(:)=0
            hgs.link_pm2Olf(:)=0
            read(itmp) (hgs.link_Olf2pm(i),i=1,hgs.nnOlf)
            read(itmp) (hgs.link_pm2Olf(i),i=1,hgs.mesh.nn)
	        call freeunit(itmp)

            ! element data
            FName=trim(hgs.prefix)//'o.elements_olf'
            call Msg('Reading HGS OLF element file: '//fname)

	        call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')

		    read(itmp) hgs.mesh.OlfNLN
		    read(itmp) hgs.mesh.nolfe
		    allocate(hgs.mesh.inolf(hgs.mesh.OlfNLN,hgs.mesh.nolfe), &
			    hgs.iolf_id_elem(hgs.mesh.nolfe), &
			    hgs.iolf_3d_elem_map(hgs.mesh.nolfe), &
			    stat=ialloc)
		    call AllocChk(ialloc,'Overland element arrays')
		    hgs.mesh.inolf = 0 ! automatic initialization
		    hgs.iolf_id_elem = 0 ! automatic initialization
		    hgs.iolf_3d_elem_map = 0 ! automatic initialization
		    read(itmp) ((hgs.mesh.inolf(j,i),j=1,hgs.mesh.OlfNLN),i=1,hgs.mesh.nolfe)
		    read(itmp) (hgs.iolf_id_elem(i),i=1,hgs.mesh.nolfe)
		    read(itmp) (hgs.iolf_3d_elem_map(i),i=1,hgs.mesh.nolfe)
		    call freeunit(itmp)
        end if

        
        FName=trim(hgs.prefix)//'o.coordinates_frac'
        inquire(file=FName,exist=FileExists)
        if(FileExists) then
            call Msg('Reading HGS fracture coordinate file: '//fname)
            
            call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')
	        read(itmp) hgs.nnfrac
            read(itmp) hgs.frac_scheme

            allocate(hgs.link_frac2pm(hgs.nnfrac), &
                hgs.link_pm2frac(hgs.mesh.nn),  stat=ialloc)
            call AllocChk(ialloc,'fracture dual fractures arrays ')
            hgs.link_frac2pm(:)=0
            hgs.link_pm2frac(:)=0
            read(itmp) (hgs.link_frac2pm(i),i=1,hgs.nnfrac)
            read(itmp) (hgs.link_pm2frac(i),i=1,hgs.mesh.nn)
	        call freeunit(itmp)

            ! element data
            FName=trim(hgs.prefix)//'o.elements_olf'
            call Msg('Reading HGS fracture element file: '//fname)
	        call getunit(itmp)
            open(itmp,file=fname,status='unknown',form='unformatted')

            ! RGM Sept 2012:  Trying to get Andra/GEOS-ITESM tetrahedra to run but hgs has many checks like this:
            !        if(inf(4,iel) .eq. 0) then
            !          nlnloc = 3
            !        else
            !          nlnloc = 4
            !        end if
            !  For now, I will force .mesh.NLNF to be 4, and set inf(4,iel)=0 for tetrahedra.
            !  I am commenting out this...
            !        if(tetramesh) then
            !            .mesh.NLNF=3
            !        else
            !            .mesh.NLNF=4
            !        end if
            !  ... and using this
            hgs.mesh.NLNF=4
            !  And now set inf(4,:) for tetrahedra
            if(tetramesh) then
		        hgs.mesh.inf(4,:)=0
	        end if

            read(itmp) hgs.mesh.NLNF
            read(itmp) hgs.mesh.nef

            allocate(hgs.mesh.inf(hgs.mesh.NLNF,hgs.mesh.nef), &
			    hgs.ifrac_id_elem(hgs.mesh.nef),  &
			    hgs.ifrac_3d_elem_map(hgs.mesh.nef,2), &
			    hgs.ap(hgs.mesh.nef), &
                stat=ialloc)
            call AllocChk(ialloc,'fracture hgs.mesh.nef arrays ')
            hgs.mesh.inf = 0 ! automatic initialization
            hgs.ifrac_id_elem = 0 ! automatic initialization
            hgs.ifrac_3d_elem_map(:,:) = 0 ! automatic initialization
            hgs.ap = 0 ! automatic initialization
            !read(itmp) ((inf(j,i),j=1,4),i=1,hgs.mesh.nef)      !DB apr-07 commented
            
            ! RGM sept 2012: see notes in grok re: tretrahedra and fracture inf
		    read(itmp) ((hgs.mesh.inf(j,i),j=1,hgs.mesh.NLNF),i=1,hgs.mesh.nef) 

            read(itmp) (hgs.ifrac_id_elem(i),i=1,hgs.mesh.nef)
		    read(itmp) ((hgs.ifrac_3d_elem_map(i,j),i=1,hgs.mesh.nef),j=1,2)
            read(itmp) (hgs.ap(i),i=1,hgs.mesh.nef)
            call freeunit(itmp)
                
            ! compute nzones_fprop
            hgs.nzones_fprop=0
            do i=1,hgs.mesh.nef
                if(hgs.ifrac_id_elem(i) > hgs.nzones_fprop) hgs.nzones_fprop=hgs.ifrac_id_elem(i)
            end do
        end if

        !call user_size_check(hgs.nzones_fprop,user_zones,user_zones_str)

                
            
          !  else if(index(HGS_CMD, HGS_face_segment_info_CMD)  /= 0) then
          !      read(FnumTG,'(a)') fname
          !      inquire(file=fname,exist=FileExists)
          !      if(.not. FileExists) then
          !          call ErrMsg('No file found: '//fname)
          !      end if
          !      call Msg('		Face info file: '//fname)
          !  
		        !!     Read face info
		        !call getunit(itmp)
          !      open(itmp,file=fname,status='unknown',form='unformatted')
		        !read(itmp) hgs.nface
		        !allocate(hgs.face_node(hgs.nface,4),hgs.face_el(hgs.nface,2),hgs.face_is(hgs.nface),stat=ialloc)
		        !call AllocChk(ialloc,'Read 3d grid face arrays')
		        !hgs.face_node = 0 ! automatic initialization
		        !hgs.face_el = 0 ! automatic initialization
		        !hgs.face_is=0
		        !read(itmp) ((hgs.face_node(i,j),j=1,4),i=1,hgs.nface)
		        !read(itmp) ((hgs.face_el(i,j),j=1,2),i=1,hgs.nface)
		        !read(itmp) (hgs.face_is(i),i=1,hgs.nface)
		        !call freeunit(itmp)
    		    !
	         !!   if(overland_flow) then
		        !!    ! have to clear this or overland new zone does not work
          !!          do i=1,hgs.nface
			       !!     call clear(face_is(i),a_olf_el)
		        !!    end do
          !!
          !!
		        !!    allocate(face_olf_el(hgs.nface),stat=ialloc)
		        !!    call AllocChk(ialloc,'calc_3d_face overland flow array')
		        !!    face_olf_el(: ) =0
		        !!end if
          !!
          !
		        !!     Read segment info
          !      read(FnumTG,'(a)') fname
          !      inquire(file=fname,exist=FileExists)
          !      if(.not. FileExists) then
          !          call ErrMsg('No file found: '//fname)
          !      end if
          !      call Msg('		Segment info file: '//fname)
		        !call getunit(itmp)
          !      open(itmp,file=fname,status='unknown',form='unformatted')
		        !read(itmp) hgs.nseg
		        !allocate(hgs.seg_node(hgs.nseg,2),stat=ialloc)
		        !call AllocChk(ialloc,'Read 3d grid seg arrays')
		        !hgs.seg_node = 0 ! automatic initialization
		        !read(itmp) ((hgs.seg_node(i,j),j=1,2),i=1,hgs.nseg)
		        !!rt 20010620
		        !read(itmp) hgs.nseg2d ! to be able to choose faces later
		        !call freeunit(itmp)
          !
		        !hgs.segments_calculated=.true.
		        !hgs.faces_calculated=.true.
          !
       !     elseif(index(HGS_CMD, HGS_EndComponent_CMD)  /= 0) then
			    !call Msg('EXIT GridComponent')
       !         exit read_GridComponent_instructions
       !     else
			    !call ErrMsg('HGSComponent: Unrecognized instruction')
       !     end if
       !
       ! end do read_GridComponent_instructions
       !
       ! 10 continue


	    return
    end subroutine HGS_GetMeshComponents

    !------------------------------------------------------------------------

    subroutine HGS_ToTecplot(FnumTG, hgs)
        implicit none
        type (HGSProject) hgs
        
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        
        integer :: i, j

        
        ! tecplot output file name
        FName=trim(hgs.prefix)//'_pm.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'Tecplot output file: '//FName)
        
        write(FNum,'(a)')'variables="X","Y","Z"'
        write(FNum,'(a,i8,a,i8,a)')'ZONE t="HGS Porous media" ,N=',hgs.mesh.nn,', E=',hgs.mesh.ne,', datapacking=block, &
                zonetype=febrick'

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (hgs.mesh.x(i),i=1,hgs.mesh.nn)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (hgs.mesh.y(i),i=1,hgs.mesh.nn)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (hgs.mesh.z(i),i=1,hgs.mesh.nn)


        do i=1,hgs.mesh.ne
            if(hgs.mesh.nln==8) then
                write(FNum,'(4i8)') (hgs.mesh.in(j,i),j=1,hgs.mesh.nln)
            else if(hgs.mesh.nln==6) then
                write(FNum,'(4i8)') (hgs.mesh.in(j,i),j=1,3), hgs.mesh.in(3,i)
                write(FNum,'(4i8)') (hgs.mesh.in(j,i),j=4,6), hgs.mesh.in(6,i)
            end if
        end do

        call FreeUnit(FNum)
       
        if(hgs.mesh.nolfe > 0) then
            FName=trim(hgs.prefix)//'_olf.dat'
            call OpenAscii(FNum,FName)
            call Msg( 'Tecplot output file: '//FName)
            write(FNum,'(a)')'variables="X","Y","Z","Aperture"'
            write(FNum,'(a,i8,a,i8,a)')'ZONE t="HydroGeoSphere Oveland flow Network" ,N=',hgs.nnolf,', E=',hgs.mesh.nolfe,', datapacking=block, &
                zonetype=fequadrilateral,  VARLOCATION=([4]=CELLCENTERED )'

            write(FNum,'(a)') '# x'
            write(FNum,'(5e20.12)') (hgs.mesh.x(i),i=1,hgs.nnolf)
            write(FNum,'(a)') '# y'
            write(FNum,'(5e20.12)') (hgs.mesh.y(i),i=1,hgs.nnolf)
            write(FNum,'(a)') '# z'
            write(FNum,'(5e20.12)') (hgs.mesh.z(i),i=1,hgs.nnolf)
            write(FNum,'(a)') '# aper'
            write(FNum,'(5e20.5)') (1,i=1,hgs.mesh.nolfe)  ! hgs.ap below was undefined
            !write(FNum,'(5e20.5)') (hgs.ap(i),i=1,hgs.mesh.nolfe)

            do i=1,hgs.mesh.nolfe
		        if(hgs.mesh.inolf(4,i) > 0) then ! quad
			        write(FNum,'(4i10)') (hgs.mesh.inolf(j,i),j=1,4)
		        else ! triangle
			        write(FNum,'(4i10)') hgs.mesh.inolf(1,i),hgs.mesh.inolf(2,i),hgs.mesh.inolf(3,i),hgs.mesh.inolf(3,i)
		        end if
            end do
        end if
       
        if(hgs.mesh.nef > 0) then
            FName=trim(hgs.prefix)//'_frac.dat'
            call OpenAscii(FNum,FName)
            call Msg( 'Tecplot output file: '//FName)
            write(FNum,'(a)')'variables="X","Y","Z","Aperture"'
            write(FNum,'(a,i8,a,i8,a)')'ZONE t="HydroGeoSphere Discrete Fracture Network" ,N=',hgs.mesh.nn,', E=',hgs.mesh.nef,', datapacking=block, &
                zonetype=fequadrilateral,  VARLOCATION=([4]=CELLCENTERED )'

            write(FNum,'(a)') '# x'
            write(FNum,'(5e20.12)') (hgs.mesh.x(i),i=1,hgs.mesh.nn)
            write(FNum,'(a)') '# y'
            write(FNum,'(5e20.12)') (hgs.mesh.y(i),i=1,hgs.mesh.nn)
            write(FNum,'(a)') '# z'
            write(FNum,'(5e20.12)') (hgs.mesh.z(i),i=1,hgs.mesh.nn)
            write(FNum,'(a)') '# aper'
            write(FNum,'(5e20.5)') (hgs.ap(i),i=1,hgs.mesh.nef)

            do i=1,hgs.mesh.nef
		        if(hgs.mesh.inf(4,i) > 0) then ! quad
			        write(FNum,'(4i10)') (hgs.mesh.inf(j,i),j=1,4)
		        else ! triangle
			        write(FNum,'(4i10)') hgs.mesh.inf(1,i),hgs.mesh.inf(2,i),hgs.mesh.inf(3,i),hgs.mesh.inf(3,i)
		        end if
            end do

            write(FNum,'(a,f10.4,a)') 'zone T="P<sub>32</sub>: ',hgs.P32,'"'
	        write(FNum,*) hgs.mesh.x(1), hgs.mesh.y(1),hgs.mesh.z(1),0
            write(FNum,'(a,a,a)') 'zone T="',trim(hgs.name),'"'
	        write(FNum,*) hgs.mesh.x(1), hgs.mesh.y(1),hgs.mesh.z(1),0
    
        end if


    end subroutine hgs_ToTecplot
!    subroutine HGS_GetMeshComponents_PMCoor(fname,hgs)
!        implicit none
!
!        type (HGSProject) hgs
!        
!        integer :: i
!        character(MAXLBL) :: fname
!    
!    
!	    call getunit(itmp)
!        open(itmp,file=fname,status='unknown',form='unformatted')
!        read(itmp) hgs.mesh.nn
!        allocate(hgs.mesh.x(hgs.mesh.nn),hgs.mesh.y(hgs.mesh.nn),hgs.mesh.z(hgs.mesh.nn), stat=ialloc)
!        call AllocChk(ialloc,'Read 3d grid node arrays')
!        hgs.mesh.x = 0 ! automatic initialization
!        hgs.mesh.y = 0 ! automatic initialization
!        hgs.mesh.z = 0 ! automatic initialization
!        read(itmp) (hgs.mesh.x(i),hgs.mesh.y(i),hgs.mesh.z(i),i=1,hgs.mesh.nn)
!        read(itmp) hgs.nx,hgs.ny,hgs.nz,hgs.nsptot
!        hgs.nsheet=hgs.nz
!	    read(itmp) hgs.ne2d  ! for ET stuff
!	    read(itmp) tetramesh	!DB feb-07
!        read(itmp) gal_mode	!DB may-07
!	    read(itmp) do_write_face_seg
!	    read(itmp) hgs.nb2d
!	    call freeunit(itmp)
!        
!    end subroutine HGS_GetMeshComponents_PMCoor
!
!    subroutine HGS_GetMeshComponents_PMElem(fname,hgs)
!        implicit none
!
!        type (HGSProject) hgs
!        
!        integer :: i, j
!        character(MAXLBL) :: fname
!
!        call getunit(itmp)
!        open(itmp,file=fname,status='unknown',form='unformatted')
!        read(itmp) hgs.mesh.nln
!        read(itmp) hgs.mesh.ne
!        allocate(hgs.mesh.in(hgs.mesh.nln,hgs.mesh.ne),stat=ialloc)
!        call AllocChk(ialloc,'Read 3d grid element arrays')
!        hgs.mesh.in = 0 ! automatic initialization
!        read(itmp) ((hgs.mesh.in(j,i),j=1,hgs.mesh.nln),i=1,hgs.mesh.ne)
!        allocate(hgs.iprop(hgs.mesh.ne),stat=ialloc)
!        call AllocChk(ialloc,'Read 3d grid element property array')
!        hgs.iprop = 0 ! automatic initialization
!        read(itmp) (hgs.iprop(i),i=1,hgs.mesh.ne)
!	    call freeunit(itmp)
!        
!   end subroutine HGS_GetMeshComponents_PMElem
!    
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadNodalOutput(FnumTG,hgs)
!        implicit none
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        !call OpenAscii(FNum,FName)
!        call Msg( 'Nodal output file: '//FName)
!        
!        if(.not. allocated(hgs.head)) allocate(hgs.head(hgs.mesh.nn))
!        
!        call read_var8(hgs.mesh.nn,FName,hgs.head,hgs.message)
!        
!        continue
!
!   end subroutine HGS_ReadNodalOutput
!    !------------------------------------------------------------------------
!   subroutine HGS_WriteNodalOutput(FnumTG,hgs)
!        implicit none
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        !call OpenAscii(FNum,FName)
!        call Msg( 'Nodal output file: '//FName)
!        
!        if(.not. allocated(hgs.head)) allocate(hgs.head(hgs.mesh.nn))
!        
!        hgs.message='-999.0'
!        
!        call write_var8(hgs.mesh.nn,FName,hgs.head,hgs.message)
!        
!        continue
!
!   end subroutine HGS_WriteNodalOutput
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadNodalOutput4(FnumTG,hgs)
!        implicit none
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!
!        
!
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Nodal output file: '//FName)
!        
!        allocate(hgs.sat(hgs.mesh.nn))
!        
!        call read_var4(hgs.mesh.nn,FName,hgs.sat,hgs.message)
!        
!        continue
!
!    end subroutine HGS_ReadNodalOutput4
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadElemK(FnumTG,hgs)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(80) :: message
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        inquire(file=FName,exist=FileExists)
!        if(.not. FileExists) then
!            call Msg( 'No HGS variable element K file: '//trim(FName))
!            return
!        end if
!
!        call OpenBinary(FNum,FName)
!        call Msg( 'HGS variable element K file: '//FName)
!        
!        allocate(hgs.ElemKx(hgs.mesh.ne),hgs.ElemKy(hgs.mesh.ne),hgs.ElemKz(hgs.mesh.ne))
!        
! 	    read(Fnum) message
!        call Msg( 'HGS variable element K file header: '//trim(message))
!	    do i=1,hgs.mesh.ne
!		    read(Fnum) hgs.ElemKx(i),hgs.ElemKy(i),hgs.ElemKz(i)
!	    end do
!	    call freeunit(FNum)
!
!    end subroutine HGS_ReadElemK
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadElemPor(FnumTG,hgs)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(80) :: message
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        call OpenBinary(FNum,FName)
!        call Msg( 'HGS variable element porosity file: '//FName)
!        
!        allocate(hgs.ElemPor(hgs.mesh.ne))  
!        
! 	    read(Fnum) message
!        call Msg( 'HGS variable element porosity file header: '//trim(message))
!		read(Fnum) (hgs.ElemPor(i),i=1,hgs.mesh.ne)
!	    call freeunit(FNum)
!
!    end subroutine HGS_ReadElemPor
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadElemStor(FnumTG,hgs)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(80) :: message
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        call OpenBinary(FNum,FName)
!        call Msg( 'HGS variable element specific storage file: '//FName)
!        
!        allocate(hgs.ElemStor(hgs.mesh.ne)) 
!        
! 	    read(Fnum) message
!        call Msg( 'HGS variable element specific storage file header: '//trim(message))
!		read(Fnum) (hgs.ElemStor(i),i=1,hgs.mesh.ne)
!	    call freeunit(FNum)
!
!    end subroutine HGS_ReadElemStor
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadElemRech(FnumTG,hgs)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'HGS variable element recharge file: '//FName)
!        
!        allocate(hgs.ElemRech(hgs.ne2d))  
!        
!		do i=1,hgs.ne2d
!            read(Fnum,*) hgs.ElemRech(i)
!        end do    
!	    call freeunit(FNum)
!
!    end subroutine HGS_ReadElemrech
!   !------------------------------------------------------------------------
!    subroutine HGS_ReadWatBalFile(FnumTG, hgs)
!        implicit none
!        
!        integer :: i, j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!        
!        integer :: i1, i2
!
!        ! WatBal file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Water_balance file: '//trim(fname))
!
!        read(FNum,'(a)',iostat=status)  hgs.TitleLine
!        
!        read(FNum,'(a)',iostat=status)  hgs.VarLine
!        ! If last character is a comma, change it to a blank (water_balance.dat files have an extra comma)
!        !if( hgs.VarLine(len_trim( hgs.VarLine):len_trim( hgs.VarLine))==',')  hgs.VarLine(len_trim( hgs.VarLine):len_trim( hgs.VarLine))=' '
!        hgs.nVars=CountChars(COMMA, hgs.VarLine(index( hgs.VarLine,'='):)) ! subract 1 for time (1st column)
!        
!        allocate(hgs.Varname(hgs.nVars),stat=ialloc)
!	    call AllocChk(ialloc,'allocate WatBal var arrays')
!
!        i1=index(hgs.VarLine,'VARIABLES =')+12
!        do i=1,hgs.nVars
!            i2=index(hgs.VarLine(i1:),'"')-2
!            read(hgs.VarLine(i1:i1+i2),'(a)') hgs.VarName(i)
!            i1=i1+i2+4
!            !hgs.VarLine=hgs.VarLine(i1:)
!            continue
!        end do
!
!
!        read(FNum,'(a)',iostat=status)  hgs.ZoneLine
!        
!        ! count nLines
!        hgs.nLines=0
!        do 
!            read(FNum,'(a)',iostat=status) LongTmpSTR
!            
!            if(status/=0) then
!                exit
!            end if
!            
!            hgs.nLines=hgs.nLines+1
!        end do
!            
!        allocate(hgs.SimTime(hgs.nLines),hgs.Rate(hgs.nLines,2:hgs.nVars),stat=ialloc)
!	    call AllocChk(ialloc,'allocate WatBal data arrays')
!        hgs.SimTime(:) = 0.0d0
!        hgs.Rate(:,:) = 0.0d0  
!        
!        rewind(FNum)
!        read(FNum,'(a)',iostat=status)  hgs.TitleLine
!        read(FNum,'(a)',iostat=status)  hgs.VarLine
!        read(FNum,'(a)',iostat=status)  hgs.ZoneLine
!        do i=1,hgs.nLines
!            read(FNum,*) hgs.SimTime(i), (hgs.Rate(i,j),j=2,hgs.nVars)
!        end do
!        
!        continue
!
!    end subroutine HGS_ReadWatBalFile
!    !------------------------------------------------------------------------
!   subroutine HGS_CompareKFields(FnumTG,hgs)
!        ! hgs is the HGS porous media data set that was loaded by conventional HGS instruction "read components"
!        ! we will create and load another data set (hgs2) to compare it to.
!        implicit none
!        
!        integer :: i,j,k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!        real(dr) :: bxr(4), byr(4)
!
!        type (HGSProject) hgs
!        type (HGSProject) hgs2
!        
!        real, allocatable :: elist(:)
!        
!        integer :: elem3d, elem3d_2
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'K comparison output file: '//FName)
!
!        
!        call HGS_GetMeshComponents(FnumTG, hgs2)
!        call HGS_ReadElemK(FnumTG, hgs2)
!        
!        continue
!        
!        if(hgs.mesh.ne==hgs2.mesh.ne) then ! they have the same number of elements.  Compare them without regard to geometry.....!!!!
!
!            write(FNum,'(a)') 'variables = "x", "y", "z","Ratio Kx1/Kx2","Ratio Ky1/Ky2","Ratio Kz1/Kz2"'
!            
!            do i=1,hgs.mesh.ne
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                write(FNum,'(6e20.12)') xe, ye, ze,hgs.ElemKx(i)/hgs2.ElemKx(i),hgs.ElemKy(i)/hgs2.ElemKy(i),hgs.ElemKz(i)/hgs2.ElemKz(i) 
!            end do
!            
!        else
!            ! assuming a layered structure of 2D meshes
!            ! loop over ne2d and form a list of corresponding element numbers from the second mesh.
!            allocate(elist(hgs.ne2d))
!            loop1: do i=1,hgs.ne2d
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                call PercentDone(i,hgs.ne2d)
!
!                    
! 			    do  j=1,hgs2.ne2d   
!                    do k=1,3
!                        bxr(k)=hgs.mesh.x(hgs.mesh.in(k,j))
!                        byr(k)=hgs.mesh.y(hgs.mesh.in(k,j))
!                    end do
!                    bxr(4)=bxr(1)
!                    byr(4)=byr(1)
!
!				    if(in_poly(4,bxr,byr,xe,ye)) then
!					    elist(i)=j 
!                        cycle loop1
!				    end if 
!			    end do 
!
!            
!            end do loop1
!            ! loop over layers and write the differences
!            ! elist(i) contains the corresponding 2d element number from the second hgs domain
!            
!            write(FNum,'(a)') 'variables = "x", "y","Ratio Kx1/Kx2","Ratio Ky1/Ky2","Ratio Kz1/Kz2"'
!
!            do i=1,hgs.nsheet-1   ! loop over the layers
!                write(TMPStr,'(a,i3,a,i3,a)') 'zone  t=" Feflow Layer ',hgs.nsheet-i,' (HGS Layer ',i,')"'
!                write(FNum,'(a)') trim(TMPStr)
!                do j=1, hgs.ne2d
!                    call find_elem_centroid(hgs.mesh,j,xe,ye,ze)
!                    elem3d=(i-1)*hgs.ne2d+j
!                    elem3d_2=(i-1)*hgs2.ne2d+elist(j)
!                    write(FNum,'(5e20.12)') xe, ye, hgs.ElemKx(elem3d)/hgs2.ElemKx(elem3d_2),hgs.ElemKy(elem3d)/hgs2.ElemKy(elem3d_2),hgs.ElemKz(elem3d)/hgs2.ElemKz(elem3d_2) 
!                end do
!   
!            end do
!            
!            continue
!        end if
!
!        call FreeUnit(FNum)
!              
!
!    end subroutine HGS_CompareKFields
!    !------------------------------------------------------------------------
!   subroutine HGS_ComparePorFields(FnumTG,hgs)
!        ! hgs is the HGS porous media data set that was loaded by conventional HGS instruction "read components"
!        ! we will create and load another data set (hgs2) to compare it to.
!        implicit none
!        
!        integer :: i,j,k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!        real(dr) :: bxr(4), byr(4)
!
!        type (HGSProject) hgs
!        type (HGSProject) hgs2
!        
!        real, allocatable :: elist(:)
!        
!        integer :: elem3d, elem3d_2
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Porosity comparison output file: '//FName)
!
!        
!        call HGS_GetMeshComponents(FnumTG, hgs2)
!        call HGS_ReadElemPor(FnumTG, hgs2)
!        
!        continue
!        
!        if(hgs.mesh.ne==hgs2.mesh.ne) then ! they have the same number of elements.  Compare them without regard to geometry.....!!!!
!
!            write(FNum,'(a)') 'variables = "x", "y", "z","Ratio Por1/Por2"'
!            
!            do i=1,hgs.mesh.ne
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                write(FNum,'(6e20.12)') xe, ye, ze,hgs.ElemPor(i)/hgs2.ElemPor(i) 
!            end do
!            
!        else
!            ! assuming a layered structure of 2D meshes
!            ! loop over ne2d and form a list of corresponding element numbers from the second mesh.
!            allocate(elist(hgs.ne2d))
!            loop1: do i=1,hgs.ne2d
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                call PercentDone(i,hgs.ne2d)
!
!                    
! 			    do  j=1,hgs2.ne2d   
!                    do k=1,3
!                        bxr(k)=hgs.mesh.x(hgs.mesh.in(k,j))
!                        byr(k)=hgs.mesh.y(hgs.mesh.in(k,j))
!                    end do
!                    bxr(4)=bxr(1)
!                    byr(4)=byr(1)
!
!				    if(in_poly(4,bxr,byr,xe,ye)) then
!					    elist(i)=j 
!                        cycle loop1
!				    end if 
!			    end do 
!
!            
!            end do loop1
!            ! loop over layers and write the differences
!            ! elist(i) contains the corresponding 2d element number from the second hgs domain
!            
!            write(FNum,'(a)') 'variables = "x", "y","Ratio Por1/Por2"'
!
!            do i=1,hgs.nsheet-1   ! loop over the layers
!                write(TMPStr,'(a,i3,a,i3,a)') 'zone  t=" Feflow Layer ',hgs.nsheet-i,' (HGS Layer ',i,')"'
!                write(FNum,'(a)') trim(TMPStr)
!                do j=1, hgs.ne2d
!                    call find_elem_centroid(hgs.mesh,j,xe,ye,ze)
!                    elem3d=(i-1)*hgs.ne2d+j
!                    elem3d_2=(i-1)*hgs2.ne2d+elist(j)
!                    write(FNum,'(5e20.12)') xe, ye, hgs.ElemPor(elem3d)/hgs2.ElemPor(elem3d_2) 
!                end do
!   
!            end do
!            
!            continue
!        end if
!
!        call FreeUnit(FNum)
!              
!
!    end subroutine HGS_ComparePorFields
!    !------------------------------------------------------------------------
!   subroutine HGS_CompareStorFields(FnumTG,hgs)
!        ! hgs is the HGS Storous media data set that was loaded by conventional HGS instruction "read components"
!        ! we will create and load another data set (hgs2) to compare it to.
!        implicit none
!        
!        integer :: i,j,k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!        real(dr) :: bxr(4), byr(4)
!
!        type (HGSProject) hgs
!        type (HGSProject) hgs2
!        
!        real, allocatable :: elist(:)
!        
!        integer :: elem3d, elem3d_2
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Specific Storage comparison output file: '//FName)
!
!        
!        call HGS_GetMeshComponents(FnumTG, hgs2)
!        call HGS_ReadElemStor(FnumTG, hgs2)
!        
!        continue
!        
!        if(hgs.mesh.ne==hgs2.mesh.ne) then ! they have the same number of elements.  Compare them without regard to geometry.....!!!!
!
!            write(FNum,'(a)') 'variables = "x", "y", "z","Ratio Stor1/Stor2"'
!            
!            do i=1,hgs.mesh.ne
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                write(FNum,'(6e20.12)') xe, ye, ze,hgs.ElemStor(i)/hgs2.ElemStor(i)
!            end do
!            
!        else
!            ! assuming a layered structure of 2D meshes
!            ! loop over ne2d and form a list of corresponding element numbers from the second mesh.
!            allocate(elist(hgs.ne2d))
!            loop1: do i=1,hgs.ne2d
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                call PercentDone(i,hgs.ne2d)
!
!                    
! 			    do  j=1,hgs2.ne2d   
!                    do k=1,3
!                        bxr(k)=hgs.mesh.x(hgs.mesh.in(k,j))
!                        byr(k)=hgs.mesh.y(hgs.mesh.in(k,j))
!                    end do
!                    bxr(4)=bxr(1)
!                    byr(4)=byr(1)
!
!				    if(in_poly(4,bxr,byr,xe,ye)) then
!					    elist(i)=j 
!                        cycle loop1
!				    end if 
!			    end do 
!
!            
!            end do loop1
!            ! loop over layers and write the differences
!            ! elist(i) contains the corresponding 2d element number from the second hgs domain
!            
!            write(FNum,'(a)') 'variables = "x", "y","Ratio Stor1/Stor2"'
!
!            do i=1,hgs.nsheet-1   ! loop over the layers
!                write(TMPStr,'(a,i3,a,i3,a)') 'zone  t=" Feflow Layer ',hgs.nsheet-i,' (HGS Layer ',i,')"'
!                write(FNum,'(a)') trim(TMPStr)
!                do j=1, hgs.ne2d
!                    call find_elem_centroid(hgs.mesh,j,xe,ye,ze)
!                    elem3d=(i-1)*hgs.ne2d+j
!                    elem3d_2=(i-1)*hgs2.ne2d+elist(j)
!                    write(FNum,'(5e20.12)') xe, ye, hgs.ElemStor(elem3d)/hgs2.ElemStor(elem3d_2) 
!                end do
!   
!            end do
!            
!            continue
!        end if
!
!        call FreeUnit(FNum)
!              
!
!    end subroutine HGS_CompareStorFields
!    !------------------------------------------------------------------------
!   subroutine HGS_CompareRechFields(FnumTG,hgs)
!        ! hgs is the HGS porous media data set that was loaded by conventional HGS instruction "read components"
!        ! we will create and load another data set (hgs2) to compare it to.
!        implicit none
!        
!        integer :: i,j,k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!        real(dr) :: bxr(4), byr(4)
!
!        type (HGSProject) hgs
!        type (HGSProject) hgs2
!        
!        real, allocatable :: elist(:)
!        
!        integer :: elem3d, elem3d_2
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Recharge comparison output file: '//FName)
!
!        
!        call HGS_GetMeshComponents(FnumTG, hgs2)
!        call HGS_ReadElemRech(FnumTG, hgs2)
!        
!        continue
!        
!        if(hgs.mesh.ne==hgs2.mesh.ne) then ! they have the same number of elements.  Compare them without regard to geometry.....!!!!
!
!            write(FNum,'(a)') 'variables = "x", "y", "z","Ratio Rech1/Rech2"'
!            
!            do i=1,hgs.mesh.ne
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                write(FNum,'(6e20.12)') xe, ye, ze,hgs.ElemRech(i)/hgs2.ElemRech(i) 
!            end do
!            
!        else
!            ! assuming a layered structure of 2D meshes
!            ! loop over ne2d and form a list of corresponding element numbers from the second mesh.
!            allocate(elist(hgs.ne2d))
!            loop1: do i=1,hgs.ne2d
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                call PercentDone(i,hgs.ne2d)
!
!                    
! 			    do  j=1,hgs2.ne2d   
!                    do k=1,3
!                        bxr(k)=hgs.mesh.x(hgs.mesh.in(k,j))
!                        byr(k)=hgs.mesh.y(hgs.mesh.in(k,j))
!                    end do
!                    bxr(4)=bxr(1)
!                    byr(4)=byr(1)
!
!				    if(in_poly(4,bxr,byr,xe,ye)) then
!					    elist(i)=j 
!                        cycle loop1
!				    end if 
!			    end do 
!
!            
!            end do loop1
!            ! loop over top layer and write the differences
!            ! elist(i) contains the corresponding 2d element number from the second hgs domain
!            
!            write(FNum,'(a)') 'variables = "x", "y","Ratio Rech1/Rech2"'
!
!            i=hgs.nsheet-1   ! top layer
!            write(TMPStr,'(a,i3,a,i3,a)') 'zone  t=" Feflow Layer ',hgs.nsheet-i,' (HGS Layer ',i,')"'
!            write(FNum,'(a)') trim(TMPStr)
!            do j=1, hgs.ne2d
!                call find_elem_centroid(hgs.mesh,j,xe,ye,ze)
!                elem3d=j
!                elem3d_2=elist(j)
!                write(FNum,'(5e20.12)') xe, ye, hgs.ElemRech(elem3d)/hgs2.ElemRech(elem3d_2) 
!            end do
!   
!            
!            continue
!        end if
!
!        call FreeUnit(FNum)
!              
!
!    end subroutine HGS_CompareRechFields
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadNodalAscii(FnumTG,hgs)
!        implicit none
!        
!        integer :: j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (HGSProject) hgs
!
!        
!
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Nodal output file: '//FName)
!        
!        allocate(hgs.head(hgs.mesh.nn))
!        
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!	    end if
!	    read(itmp,*) (hgs.head(j),j=1,hgs.mesh.nn)
!	    call freeunit(FNum)
!        
!        continue
!
!    end subroutine HGS_ReadNodalAscii
!   !------------------------------------------------------------------------
!   subroutine HGS_ReadNodeList(FnumTG,hgs)
!        implicit none
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        
!        character(MAXLBL) :: line
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Node list file: '//FName)
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        end if
!        
!        if(hgs.nNodeList==0) then
!            allocate(hgs.NodeListSize(10),hgs.NodeList(10,hgs.mesh.nn))
!            hgs.NodeListSize(:)=0
!            hgs.NodeList(:,:)=0
!        end if
!        
!        hgs.nNodeList=hgs.nNodeList+1
!        
!        do 
!	        read(FNum,'(a)',iostat=status) line
!            if(status/=0) then
!                exit
!            end if
!            
!            hgs.NodeListSize(hgs.nNodeList)=hgs.NodeListSize(hgs.nNodeList)+1
!            read(line,*) hgs.NodeList(hgs.nNodeList,hgs.NodeListSize(hgs.nNodeList))
!            
!        end do    
!	    call freeunit(FNum)
!        
!        continue
!
!   end subroutine HGS_ReadNodeList
!      !------------------------------------------------------------------------
!   subroutine HGS_ReadElementList(FnumTG,hgs)
!        implicit none
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        
!        character(MAXLBL) :: line
!
!        type (HGSProject) hgs
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Element list file: '//FName)
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        end if
!        
!        if(hgs.nElementList==0) then
!            allocate(hgs.ElementListSize(10),hgs.ElementList(10,hgs.mesh.nn))
!            hgs.ElementListSize(:)=0
!            hgs.ElementList(:,:)=0
!        end if
!        
!        hgs.nElementList=hgs.nElementList+1
!        
!        do 
!	        read(FNum,'(a)',iostat=status) line
!            if(status/=0) then
!                exit
!            end if
!            
!            hgs.ElementListSize(hgs.nElementList)=hgs.ElementListSize(hgs.nElementList)+1
!            read(line,*) hgs.ElementList(hgs.nElementList,hgs.ElementListSize(hgs.nElementList))
!            
!        end do    
!	    call freeunit(FNum)
!        
!        continue
!
!    end subroutine HGS_ReadElementList
!
!    !------------------------------------------------------------------------
!   subroutine HGS_ReadFluidProps(FnumTG)
!        implicit none
!
!        integer :: FnumTG
!        integer :: Fnum, FnumFluidProps
!        character(MAXLBL) :: FName
!
!        
!        character(61), parameter :: kg_m_s_str			=   'units: kilogram-metre-second'
!        character(60), parameter :: kg_m_min_str		=   'units: kilogram-metre-minute'
!        character(61), parameter :: kg_m_h_str			=   'units: kilogram-metre-hour'
!        character(61), parameter :: kg_m_d_str			=   'units: kilogram-metre-day'
!        character(61), parameter :: kg_m_y_str			=   'units: kilogram-metre-year'
!        character(61), parameter :: kg_cm_s_str			=   'units: kilogram-centimetre-second'
!        character(61), parameter :: kg_cm_min_str		=   'units: kilogram-centimetre-minute'
!        character(61), parameter :: kg_cm_h_str			=   'units: kilogram-centimetre-hour'
!        character(61), parameter :: kg_cm_d_str			=   'units: kilogram-centimetre-day'
!        character(61), parameter :: kg_cm_y_str			=   'units: kilogram-centimetre-year'
!        character(60), parameter :: chng_rho            =   'reference fluid density'
!        character(60), parameter :: chng_visc           =   'reference fluid viscosity'
!        character(60), parameter :: chng_grav           =   'gravitational acceleration'
!        character(60) :: HGS_EndFluidProps_CMD=	'end fluid props'
!
!       
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Fluid Properties file: '//FName)
!
!        ! Create one processed input file
!        call openascii(FnumFluidProps,'FluidProps.input')
!
!        ! strip out blanks and comments and concatenate included files
!        call StripComments(Fnum,FnumFluidProps)
!	    call freeunit(Fnum)
!
!        
!        read_FluidProps_instructions: do
!            read(FnumFluidProps,'(a)',iostat=status,end=10) HGS_CMD
!            call Msg('!-----------------------')
!            call Msg('HGS:'//HGS_CMD)
!
!            if(status/=0) then
! 		        write(ErrStr,'(a)') 'File: '//'?file??'
!		        l1=len_trim(ErrStr)
!		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
!			    call ErrMsg(ErrStr)
!            end if
!            
!            if (index(HGS_CMD, kg_m_s_str) /= 0) then
!                cfactor=1.
!            else if (index(HGS_CMD, kg_m_min_str) /=0) then
!                cfactor=60.
!            else if (index(HGS_CMD, kg_m_h_str) /=0) then
!                cfactor=60.*60.
!            else if (index(HGS_CMD, kg_m_d_str) /=0) then
!                cfactor=86400.
!            else if (index(HGS_CMD, kg_m_y_str) /=0) then
!                cfactor=86400.*365.
!            else if (index(HGS_CMD, kg_cm_s_str) /=0) then
!                cfactor=1.
!            else if (index(HGS_CMD, kg_cm_min_str) /=0) then
!                cfactor=60.
!            else if (index(HGS_CMD, kg_cm_h_str) /=0) then
!                cfactor=60.*60.
!            else if (index(HGS_CMD, kg_cm_d_str) /=0) then
!                cfactor=86400.
!            else if (index(HGS_CMD, kg_cm_y_str) /=0) then
!                cfactor=86400.*365.
!
!            elseif(index(HGS_CMD, chng_rho) /=0) then
!                call read_val(FnumFluidProps,'Fluid density ',rho) ! read a new one
!
!            elseif(index(HGS_CMD, chng_visc) /=0) then
!                call read_val(FnumFluidProps,'Fluid viscosity ',visc) ! read a new one
!
!            elseif(index(HGS_CMD, chng_grav) /=0) then
!                call read_val(FnumFluidProps,'Gravitational acceleration ',grav) ! read a new one
!
!
!            elseif(index(HGS_CMD, HGS_EndFluidProps_CMD)  /= 0) then
!			    call Msg('EXIT FluidProps')
!                exit read_FluidProps_instructions
!            else
!			    call ErrMsg('HGSFluidProps: Unrecognized instruction')
!            end if
!
!        end do read_FluidProps_instructions
!
!10      continue
!        
!   	    call freeunit(FnumFluidProps)
!
!
!
!    end subroutine HGS_ReadFluidProps
!    !------------------------------------------------------------------------
!    subroutine HGS_WriteComponents(FnumTG, hgs)
!        implicit none
!
!        integer :: FnumTG
!        type (HGSProject) hgs
!        
!        integer :: i, j
!        character*80 fname
!
!        character(60) :: HGS_PorousMedia_CMD="porous media"
!        character(60) :: HGS_Fracture_CMD="fracture"
!	    !character(60) :: HGS_face_segment_info_CMD=	'face/segment data'
!        character(60) :: HGS_EndComponent_CMD=	'end components'
!
!	    hgs.segments_calculated=.false.
!	    hgs.faces_calculated=.false.
!
!        write_GridComponent_instructions: do
!            read(FnumTG,'(a)',iostat=status,end=10) HGS_CMD
!            call Msg('!-----------------------')
!            call Msg('HGS:'//HGS_CMD)
!            
!            if(status/=0) then
! 		        write(ErrStr,'(a)') 'File: '//'?file??'
!		        l1=len_trim(ErrStr)
!		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
!			    call ErrMsg(ErrStr)
!            end if
!            
!            if(index(HGS_CMD, HGS_PorousMedia_CMD)  /= 0) then
!                ! coordinate data
!                read(FnumTG,'(a)') fname
!                call Msg('		Porous media coordinate file: '//fname)
!	            call getunit(itmp)
!                open(itmp,file=fname,status='unknown',form='unformatted')
!                write(itmp) hgs.mesh.nn
!                write(itmp) (hgs.mesh.x(i),hgs.mesh.y(i),hgs.mesh.z(i),i=1,hgs.mesh.nn)
!                write(itmp) hgs.nx,hgs.ny,hgs.nz,hgs.nsptot
!	            write(itmp) hgs.ne2d  ! for ET stuff
!	            write(itmp) tetramesh	!DB feb-07
!                write(itmp) gal_mode	!DB may-07
!	            write(itmp) do_write_face_seg
!	            write(itmp) hgs.nb2d
!	            call freeunit(itmp)
!                
!                ! element data
!                read(FnumTG,'(a)') fname
!                call msg('		Porous media element file: '//fname)
!	            call getunit(itmp)
!                open(itmp,file=fname,status='unknown',form='unformatted')
!                write(itmp) hgs.mesh.nln
!                write(itmp) hgs.mesh.ne
!                write(itmp) ((hgs.mesh.in(j,i),j=1,hgs.mesh.nln),i=1,hgs.mesh.ne)
!                write(itmp) (hgs.iprop(i),i=1,hgs.mesh.ne)
!	            call freeunit(itmp)
!	        
!                !read_grid_components_pm    = .true.
!
!        
!            else if(index(HGS_CMD, HGS_Fracture_cmd)  /= 0) then
!                ! coordinate data
!                read(FnumTG,'(a)') fname
!                call Msg('		Fracture coordinate file: '//fname)
!                call getunit(itmp)
!                open(itmp,file=fname,status='unknown',form='unformatted')
!	            write(itmp) hgs.nnfrac
!                write(itmp) hgs.frac_scheme
!                write(itmp) (hgs.link_frac2pm(i),i=1,hgs.nnfrac)
!                write(itmp) (hgs.link_pm2frac(i),i=1,hgs.mesh.nn)
!	            call freeunit(itmp)
!
!                ! element data
!                read(FnumTG,'(a)') fname
!                call Msg('		Fracture element file: '//fname)
!	            call getunit(itmp)
!                open(itmp,file=fname,status='unknown',form='unformatted')
!                write(itmp) hgs.mesh.NLNF
!                write(itmp) hgs.mesh.nef
!		        write(itmp) ((hgs.mesh.inf(j,i),j=1,hgs.mesh.NLNF),i=1,hgs.mesh.nef) 
!                write(itmp) (hgs.ifrac_id_elem(i),i=1,hgs.mesh.nef)
!		        write(itmp) ((hgs.ifrac_3d_elem_map(i,j),i=1,hgs.mesh.nef),j=1,2)
!                write(itmp) (hgs.ap(i),i=1,hgs.mesh.nef)
!                call freeunit(itmp)
!            
!          
!          ! Fix later if needed      
!          !  else if(index(HGS_CMD, HGS_face_segment_info_CMD)  /= 0) then
!          !      read(FnumTG,'(a)') fname
!          !      inquire(file=fname,exist=FileExists)
!          !      if(.not. FileExists) then
!          !          call ErrMsg('No file found: '//fname)
!          !      end if
!          !      call Msg('		Face/segment info file: '//fname)
!          !  
!		        !!     Read face info
!		        !call getunit(itmp)
!		        !open(itmp,file=LocalPrefix(:LocalPrefixLength)//'o.fac',status='unknown',form='unformatted')
!		        !write(itmp) hgs.nface
!		        !write(itmp) ((hgs.face_node(i,j),j=1,4),i=1,hgs.nface)
!		        !write(itmp) ((hgs.face_el(i,j),j=1,2),i=1,hgs.nface)
!		        !write(itmp) (hgs.face_is(i),i=1,hgs.nface)
!		        !call freeunit(itmp)
!          !
!		        !!     write segment info
!		        !call getunit(itmp)
!		        !open(itmp,file=LocalPrefix(:LocalPrefixLength)//'o.seg',status='unknown',form='unformatted')
!		        !write(itmp) hgs.nseg
!		        !write(itmp) ((hgs.seg_node(i,j),j=1,2),i=1,hgs.nseg)
!		        !write(itmp) hgs.nseg2d ! to be able to choose faces later
!		        !call freeunit(itmp)
!
!            else if(index(HGS_CMD, HGS_EndComponent_CMD)  /= 0) then
!			    call Msg('EXIT GridComponent')
!                exit write_GridComponent_instructions
!            else
!			    call ErrMsg('HGSComponent: Unrecognized instruction')
!            end if
!
!        end do write_GridComponent_instructions
!
!        10 continue
!
!        !rt 20010619
!        hgs.nn2d = hgs.mesh.nn/hgs.nz
!        hgs.ne2d = hgs.mesh.ne/(hgs.nz-1)
!
!        hgs.blockel=.true.
!        if(hgs.mesh.nln==6 .or. hgs.mesh.nln==4) hgs.blockel=.false.
!
!
!        ! compute nzones_prop
!        hgs.nzones_prop=0
!        do i=1,hgs.mesh.ne
!            if(hgs.iprop(i) > hgs.nzones_prop) hgs.nzones_prop=hgs.iprop(i)
!        end do
!        !call user_size_check(hgs.nzones_prop,user_zones,user_zones_str)
!
!        ! compute nzones_fprop
!        hgs.nzones_fprop=0
!        do i=1,hgs.mesh.nef
!            if(hgs.ifrac_id_elem(i) > hgs.nzones_fprop) hgs.nzones_fprop=hgs.ifrac_id_elem(i)
!        end do
!
!        !call user_size_check(hgs.nzones_fprop,user_zones,user_zones_str)
!
!
!	    return
!    end subroutine HGS_WriteComponents
!
!    !------------------------------------------------------------------------
!
!    subroutine HGS_WriteNodeSheetsToSurfer(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!                
!        integer :: i, j
!        integer :: node3d
!
!        
!        ! tecplot output file 
!        read(FnumTG,'(a)') FNAME
!        do i=1,hgs.nsheet
!            write(TmpSTR,'(i4.4)') i 
!            call OpenAscii(FNum,trim(fname)//'_sheet.'//trim(TMPStr)//'.dat')
!            call Msg( 'Surfer output file: '//trim(fname)//'_sheet.'//trim(TMPStr)//'.dat')
!
!            write(FNum,'(a)')' X   Y  Z   Head'
!            do j=1,hgs.nn2d
!                node3d=(i-1)*hgs.nn2d+j
!                write(FNum,'(5e20.12)') hgs.mesh.x(node3d),hgs.mesh.y(node3d),hgs.mesh.Z(node3d),hgs.head(node3d)
!            end do
!             call FreeUnit(FNum)
!   
!        end do
!
!
!    end subroutine HGS_WriteNodeSheetsToSurfer
!    !------------------------------------------------------------------------
!    subroutine HGS_AdjustZFromChosenNodesAndSheetNumber(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!                
!        integer :: i, j
!        integer :: node3d
!
!        
!        real(dr) :: ZAdjust
!        integer :: iFromSheet
!
!        read(FnumTG,*) ZAdjust
!        read(FnumTG,*) iFromSheet
!       
!         ! Now check elevation
!        do i=1,hgs.nn2d
!            if(hgs.nchos(i)) then
!                do j=iFromSheet,hgs.nsheet
!                    node3d=(j-1)*hgs.nn2d+i
!                    hgs.mesh.z(node3d)=hgs.mesh.z(node3d)+ZAdjust
!                end do
!            end if
!        end do
!
!    end subroutine HGS_AdjustZFromChosenNodesAndSheetNumber
!    !------------------------------------------------------------------------
!    subroutine HGS_AdjustSurfaceFromGBNprop(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!                
!        integer :: i, j
!        integer :: node3d
!
!        
!        real(dr) :: ZAdjust
!        integer :: iFromSheet
!
!        read(FnumTG,*) iFromSheet
!       
!         ! Now check elevation
!        do i=1,hgs.nn2d
!            node3d=(hgs.nsheet-1)*hgs.nn2d+i
!            if(abs(hgs.nprop(i)-hgs.mesh.z(node3d)) > 1.e-3) then
!                ZAdjust=hgs.nprop(i)-hgs.mesh.z(node3d)
!                do j=iFromSheet,hgs.nsheet
!                    node3d=(j-1)*hgs.nn2d+i
!                    hgs.mesh.z(node3d)=hgs.mesh.z(node3d)+ZAdjust
!                end do
!            end if
!        end do
!
!    end subroutine HGS_AdjustSurfaceFromGBNprop
!
!    !------------------------------------------------------------------------
!    subroutine HGS_ReadGBNchos(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!                
!        integer :: i
!        integer :: FnumGbNchos
!        character(MAXLBL) :: FnameGbNchos
!
!        character(80)       :: dummy 
!        
!        read(FnumTG,'(a)') FnameGbNchos
!     
!        call OpenBinary(FnumGbNchos,FnameGbNchos)
!        read(FnumGbNchos) dummy 
!        
!        allocate(hgs.nchos(hgs.nn2d), stat=ialloc)
!	    call AllocChk(ialloc,'allocate hgs nchos arrays')
!	    hgs.nchos(:)=.false.
!
!
!        read(FnumGbNchos,iostat=status) (hgs.nchos(i),i=1,hgs.nn2d)  
!	    
!        if(status /= 0) then 
!	        write(ErrStr,'(a)') 'File: '//FnameGbNchos
!	        write(ErrStr,'(a)') 'Error reading file: '//FnameGbNchos
!		    call ErrMsg(ErrStr)
!	    end if
!
!        call freeunit(FnumGbNchos)
!
!    end subroutine HGS_ReadGBNchos
!    !------------------------------------------------------------------------
!    subroutine HGS_WriteNodeSheetsGeometryToSurfer(hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: Fnum
!                
!        integer :: i, j
!        integer :: node3d
!
!        
!        do i=1,hgs.nsheet
!            write(TmpSTR,'(i4.4)') i 
!            call OpenAscii(FNum,'geometry_sheet.'//trim(TMPStr)//'.dat')
!            call Msg( 'Surfer output file: geometry_sheet.'//trim(TMPStr)//'.dat')
!
!            write(FNum,'(a)')' X   Y  Z'
!            do j=1,hgs.nn2d
!                node3d=(i-1)*hgs.nn2d+j
!                write(FNum,'(5e20.12)') hgs.mesh.x(node3d),hgs.mesh.y(node3d),hgs.mesh.z(node3d)
!            end do
!             call FreeUnit(FNum)
!   
!        end do
!
!
!    end subroutine HGS_WriteNodeSheetsGeometryToSurfer
!    !------------------------------------------------------------------------
!    subroutine HGS_WriteNodeListsByTopSheet(hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: Fnum
!                
!        integer :: i, j
!
!
!        call OpenAscii(FNum,'choose_faces_top.ginc')
!        call Msg( 'Output file: choose_faces_top.ginc')
!
!        do i=hgs.mesh.ne-hgs.ne2d+1,hgs.mesh.ne   ! loop over the top layer elements
!            write(FNum,'(a)') 'Choose face by nodes'
!            write(FNum,'(4i10)') (hgs.mesh.in(j,i),j=4,6),0
!        end do
!        call FreeUnit(FNum)
!
!
!    end subroutine HGS_WriteNodeListsByTopSheet
!    !------------------------------------------------------------------------
!    subroutine HGS_WriteElementCentroids(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!                
!        integer :: i
!	    real(dr) :: xe, ye, ze
!
!        
!        ! output file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Output file: '//trim(fname))
!
!        do i=1,hgs.mesh.ne
!		    call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!            write(FNum,'(i10,3e20.12)') i, xe, ye, ze
!        end do
!        call FreeUnit(FNum)
!
!
!    end subroutine HGS_WriteElementCentroids
!    
!    !------------------------------------------------------------------------
!    subroutine HGS_ExtractDataFromLstFile(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumDt
!        character(MAXLBL) :: FNameDt
!        integer :: FnumNewtNodes
!        character(MAXLBL) :: FNameNewtNodes
!                
!        integer :: i
!        
!        real(dr) :: Simtime,DeltaT,PcentDone
!        
!        real(dr) :: relfac,delval,resval
!        integer :: Iter,dvnode,dvncnode,rvnode,rvncnode,solv
!        
!        integer :: nDelVal(hgs.mesh.nn), nResVal(hgs.mesh.nn)
!        nDelVal(:)=0
!        nResVal(:)=0
!        
!        ! listing file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Listing file: '//trim(fname))
!
!        FNameDt=trim(fname)//'.dt.dat'
!        call OpenAscii(FNumDt,FNameDt)
!        call Msg( 'Timestep file: '//FNameDt(:len_trim(FNameDt)))
!        write(FNumDt,'(a)') 'Title = " Timestep size"'
!        write(FNumDt,'(a)') 'variables = "Time","DeltaT","Percent done"'
!        write(FNumDt,'(a)') 'Zone T='//trim(fname)
!
!        FNameNewtNodes=trim(fname)//'.NewtNodes_scatter.dat'
!        call OpenAscii(FNumNewtNodes,FNameNewtNodes)
!        call Msg( 'Timestep file: '//FNameNewtNodes(:len_trim(FNameNewtNodes)))
!        write(FNumNewtNodes,'(a)') 'Title = " Newton procedure node hits scatter plot"'
!        write(FNumNewtNodes,'(a)') 'VARIABLES = "X","Y","Z","Delval","Resval"'
!        write(FNumNewtNodes,'(a)') 'Zone T='//trim(fname)
!        
!        do 
!            read(FNum,'(a)',iostat=status) TmpSTR
!	        if(status /= 0) exit
!            
!            if(index(TmpSTR,'%done      Time             delta_t          Tnext') > 0) then ! found timestep info
!                read(FNum,'(a)') TmpSTR
!                ! extract timestep data
!                read(TmpSTR,*) PcentDone,Simtime,DeltaT
!                write(FNumDt,*) Simtime,DeltaT,PcentDone
!            else if(index(TmpSTR,'Iter Relfac         Delval  @Node NcNode        Resval  @Node NcNode Solv  Dom') > 0) then ! found newt node info
!                read(FNum,'(a)') TmpSTR  ! throw away iteration zero
!                do 
!                    read(FNum,'(a)',iostat=status) TmpSTR
!           	        if(status /= 0) exit
!                    ! extract newtnode data
!                    read(TmpSTR,*,iostat=status) Iter,relfac,delval,dvnode,dvncnode,resval,rvnode,rvncnode,solv
!           	        if(status /= 0) exit
!                    if(dvnode<=hgs.mesh.nn) then
!                        nDelVal(dvnode)=nDelVal(dvnode)+1
!                    end if
!                    if(rvnode<=hgs.mesh.nn) then
!                        nResVal(rvnode)=nResVal(rvnode)+1
!                    end if
!                end do
!            end if
!
!        end do
!        
!        do i=1,hgs.mesh.nn
!            write(FNumNewtNodes,'(3e20.12,2i10)') hgs.mesh.x(i), hgs.mesh.y(i), hgs.mesh.z(i), nDelVal(i), nResVal(i)
!        end do
!        call FreeUnit(FNum)
!        call FreeUnit(FNumDt)
!        call FreeUnit(FNumNewtNodes)
!
!
!    end subroutine HGS_ExtractDataFromLstFile
!    
!    !------------------------------------------------------------------------
!    ! Integrate flows in water balance file to total monthly values 
!    subroutine HGS_ExtractMnthlyFlowWatBalFile(FnumTG)
!        implicit none
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FNumMnthlyFlow
!        character(MAXLBL) :: FNameMnthlyFlow
!        integer :: FNumMnthlyFlow2
!        character(MAXLBL) :: FNameMnthlyFlow2
!                
!        integer :: i, nvar
!        
!        integer :: imonth, next_imonth
!        character(8) :: JulianMonthSTR, StartMonthSTR
!        real(dr) :: JulianDay, CurrentDay, DeltaTime, StartDay
!
!
!        real(dr) :: SimTime
!        real(dr), allocatable :: MnthlyIntegratedFlow(:),VarFlow(:)   
!        
!        real(dr) :: ScaleFactor     
!        
!        ! WatBal file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Water_balance file: '//trim(fname))
!
!        read(FnumTG,*) ScaleFactor  ! = 1 for m3/d, = 
!        
!
!        FNameMnthlyFlow=trim(fname)//'.MnthlyFlow.csv'
!        call OpenAscii(FNumMnthlyFlow,FNameMnthlyFlow)
!        call Msg( 'CSV output file: '//FNameMnthlyFlow(:len_trim(FNameMnthlyFlow)))
!        
!        FNameMnthlyFlow2=trim(fname)//'.MnthlyFlow.dat'
!        call OpenAscii(FNumMnthlyFlow2,FNameMnthlyFlow2)
!        call Msg( 'Tecplot output file: '//FNameMnthlyFlow2(:len_trim(FNameMnthlyFlow2)))
!
!        read(FNum,'(a)',iostat=status) TmpSTR  ! title
!        write(FNumMnthlyFlow2,'(a)') trim(TmpSTR)
!        
!        read(FNum,'(a)',iostat=status) TmpSTR  ! variables
!        write(FNumMnthlyFlow2,'(a)') trim(TmpSTR)
!        
!        ! If last character is a comma, chang it to a blank (water_balance.dat files have an extra comma)
!        if(TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))==',') TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))=' '
!        write(FNumMnthlyFlow,'(a)') TmpSTR(index(TmpSTR,'=')+1:) ! write variable names to csv
!        nvar=CountChars(COMMA,TmpSTR(index(TmpSTR,'='):)) ! subract 1 for time (1st column)
!        allocate(MnthlyIntegratedFlow(nvar), VarFlow(nvar), stat=ialloc)
!	    call AllocChk(ialloc,'allocate MnthlyIntegratedFlow arrays')
!	    MnthlyIntegratedFlow(:)=0.0d0
!	    VarFlow(:)=0.0d0
!
!        read(FNum,'(a)',iostat=status) LongTmpSTR  ! zone
!        
!        
!        read(FNum,'(a)',iostat=status) LongTmpSTR
!        read(LongTmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!        call Julian_month(SimTime,imonth,JulianDay,JulianMonthSTR)
!        CurrentDay=JulianDay
!        StartDay=JulianDay
!        StartMonthSTR=JulianMonthSTR
!
!        do 
!            read(FNum,'(a)',iostat=status) LongTmpSTR
!	        if(status /= 0) exit
!
!	        read(LongTmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!            call Julian_month(SimTime,next_imonth,JulianDay,JulianMonthSTR)
!            if(next_imonth > imonth) then ! starting next month
!                DeltaTime=JulianDay-CurrentDay ! time left in this month
!                if(DeltaTime > 0.0d0) then
!                    do i=1,nvar
!                        MnthlyIntegratedFlow(i)= MnthlyIntegratedFlow(i)+VarFlow(i)* DeltaTime 
!                    end do
!                end if
!                write(FNumMnthlyFlow,'(a,50(a1,f20.3))')StartMonthSTR,(',',MnthlyIntegratedFlow(i)/(JulianDay-StartDay)*ScaleFactor,i=1,nvar)
!                write(FNumMnthlyFlow2,'(f20.3,50(a1,f20.3))') CurrentDay+(JulianDay-StartDay)/2.0,(',',MnthlyIntegratedFlow(i)/(JulianDay-StartDay)*ScaleFactor,i=1,nvar)
!                !write(FNumMnthlyFlow,'(a,50(a1,f20.3))')StartMonthSTR,(',',MnthlyIntegratedFlow(i)*ScaleFactor,i=1,nvar)
!
!                imonth=next_imonth
!	            StartDay=JulianDay
!                StartMonthSTR=JulianMonthSTR
!	            
!	            MnthlyIntegratedFlow(:)=0.0d0
!                CurrentDay=JulianDay
!                DeltaTime=SimTime-CurrentDay  ! Time at start of next month
!                if(DeltaTime > 0.0d0) then
!                    do i=1,nvar
!                        MnthlyIntegratedFlow(i)= MnthlyIntegratedFlow(i)+VarFlow(i)* DeltaTime 
!                    end do
!                end if
!                CurrentDay=SimTime 
!
!            
!            else ! update total flow
!                DeltaTime=SimTime-CurrentDay
!                if(DeltaTime > 0.0d0) then
!                    do i=1,nvar
!                        MnthlyIntegratedFlow(i)= MnthlyIntegratedFlow(i)+VarFlow(i)* DeltaTime 
!                    end do
!                end if
!                CurrentDay=SimTime 
!	        
!	        
!            end if
!            continue
!	        
!            
!        end do
!        
!        call FreeUnit(FNum)
!        call FreeUnit(FNumMnthlyFlow)
!
!
!    end subroutine HGS_ExtractMnthlyFlowWatBalFile
!    !------------------------------------------------------------------------
!    subroutine HGS_WaterYearAccumulation(FnumTG, hgs)
!        implicit none
!        
!        type (HGSProject) hgs
!
!        
!        integer :: FnumTG
!        integer :: FNumCumulCSV
!        character(MAXLBL) :: FNameCumulCSV
!        integer :: FNumCumulDAT
!        character(MAXLBL) :: FNameCumulDAT
!                
!        integer :: i, j, k
!        
!
!        ! Water balance file structure
!        real(dr), allocatable :: WaterYearAccum(:)   
!        
!        real(dr) :: ScaleFactor   
!        real(dr) :: Area
!        
!        character(3) :: WaterYearStartMonth
!        integer :: MonthStartIndex
!        integer :: DayStartIndex
!        integer :: Day0
!        integer :: Day365
!        
!        
!        integer :: i1
!        integer :: iYear
!        
!        logical :: EndOfWaterYear
!        real(dr) :: dT
!
!        FNameCumulDAT='CumulativeWaterYear.dat'
!        call OpenAscii(FNumCumulDAT,FNameCumulDAT)
!        call Msg( 'Tecplot output file: '//FNameCumulDAT(:len_trim(FNameCumulDAT)))
!        write(FNumCumulDAT,'(a)') hgs.TitleLine
!        write(FNumCumulDAT,'(a)') hgs.VarLine
!        write(FNumCumulDAT,'(a)') hgs.ZoneLine
!
!        FNameCumulCSV='CumulativeWaterYear.CSV'
!        call OpenAscii(FNumCumulCSV,FNameCumulCSV)
!        call Msg( 'Tecplot output file: '//FNameCumulCSV(:len_trim(FNameCumulCSV)))
!        write(FNumCumulCSV,'(100(a,a))') (trim(hgs.VarName(i)),',',i=1,hgs.nVars)
!
!        
!        read(FnumTG,*) Area         ! domain XY projected area to convert to water depth equivalent
!        write(TMPStr,'(f15.2)') Area
!        call Msg( 'Model domain XY projected area: '//trim(TMPStr))
!        
!        
!        read(FnumTG,*) ScaleFactor  ! i.e 1 for m, 1000 for mm 
!        write(TMPStr,'(f15.2)') ScaleFactor
!        call Msg( 'Water depth scale factor: '//trim(TMPStr))
!        
!        read(FnumTG,*) WaterYearStartMonth
!        call Msg( 'Water year start month: '//trim(WaterYearStartMonth))
!        do i=1,12
!            if(index(MonthStartDaySTR(i),WaterYearStartMonth)>0) then
!                MonthStartIndex=i
!                exit
!            end if
!        end do
!        write(TMPStr,'(i8)') MonthStartIndex
!        call Msg( 'Water year start index (i.e. month number): '//trim(TMPStr))
!
!        read(FnumTG,*) DayStartIndex 
!        write(TMPStr,'(i8)') DayStartIndex
!        call Msg( 'Water year start day (0 to 30): '//trim(TMPStr))
!        
!        allocate(WaterYearAccum(2:hgs.nVars),stat=ialloc)
!	    call AllocChk(ialloc,'allocate rate accumulation array')
!        
!        WaterYears: do
!            Day0=MonthStartDay(MonthStartIndex)
!            Day365=MonthStartDay(MonthStartIndex+12)
!            EndOfWaterYear=.false.                        
!            WaterYearAccum(:) = 0.0d0
!            Year: do i=1,hgs.nLines
!                if(hgs.SimTime(i) >= Day0 .and. hgs.SimTime(i) <Day365) then  ! found start
!                    i1=index(MonthStartDaySTR(MonthStartIndex+12),'-')+1
!                    read(MonthStartDaySTR(MonthStartIndex+12)(i1:),*) iYear
!                    write(TMPStr,'(3i8)') iYear, Day0, Day365
!                    call Msg( 'Processing year, day start, day end: '//trim(TMPStr))
!                    do j=i+1,hgs.nLines
!                        if(j==i+1) then ! initialize 
!                            dT=hgs.SimTime(j)-Day0
!                        else if (hgs.SimTime(j) <= Day365) then  ! add to acccumulation
!                            dT=hgs.SimTime(j)-hgs.SimTime(j-1)
!                        else  ! end of water year
!                            dT=Day365-hgs.SimTime(j)
!                            EndOfWaterYear=.true.                        
!                        end if
!                        do k=2,hgs.nVars
!                            WaterYearAccum(k)=WaterYearAccum(k)+hgs.Rate(j,k)*dT/Area*ScaleFactor
!                        end do
!                        write(FNumCumulDAT,'(100g18.10)') hgs.SimTime(j),(WaterYearAccum(k),k=2,hgs.nVars)
!                        if(EndOfWaterYear) then
!                            write(FNumCumulCSV,'(100(g18.10,a))') iYear,',', (WaterYearAccum(k),',',k=2,hgs.nVars)
!                            exit Year
!                        end if
!                    end do
!                    if(hgs.SimTime(hgs.nLines) < Day365) then
!                        write(FNumCumulCSV,'(100(g18.10,a))') iYear,',', (WaterYearAccum(k),',',k=2,hgs.nVars)
!                        exit WaterYears
!                    end if
!                end if
!            end do Year
!            MonthStartIndex=MonthStartIndex+12
!            if(MonthStartIndex > 3001-12) then
!                call Msg('At end of water year data Jan-2150')
!                exit WaterYears 
!            end if
!        end do WaterYears
!               
!        call FreeUnit(FNumCumulDAT)
!        call FreeUnit(FNumCumulCSV)
!
!    end subroutine HGS_WaterYearAccumulation
!   !------------------------------------------------------------------------
!    ! Hardwired for Teck seepage calcs 2014_11_04
!    ! 1) Normalize draindown function
!    ! 2) Partition seepage to Central Pit Lake, South reclamation lake and East reclamation lake
!    ! 3) Time1, areaERL1, areaCPL1, areaSRL1  
!    ! 4) Time2, areaERL2, areaCPL2, areaSRL2
!    ! 5) Interpolate partitioning temporally from time1 to time2
!    
!    subroutine HGS_TeckRatesWatBalFile(FnumTG)
!        implicit none
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FNumNrmlDrainDown
!        character(MAXLBL) :: FNameNrmlDrainDown
!        integer :: FNumNrmlDrainDownCSV
!        character(MAXLBL) :: FNameNrmlDrainDownCSV
!                
!        integer :: i, nvar
!        
!
!        real(dr) :: SimTime
!        real(dr), allocatable :: MnthlyIntegratedFlow(:),VarFlow(:)   
!        
!        real(dr) ::  Time1, areaERL1, areaCPL1, areaSRL1    
!        real(dr) ::  Time2, areaERL2, areaCPL2, areaSRL2
!        real(dr) ::  Var, ValStart, ValEnd
!        real(dr) :: ReadStart
!        real(dr) :: Readend
!        real(dr) :: NrmlSeepage
!        real(dr) :: TFactor
!        real(dr) :: LPerS
!        real(dr) :: ToERL
!        real(dr) :: ToCPL
!        real(dr) :: ToSRL
!        real(dr) :: Slope
!        real(dr) :: LastSeepage, LastTime
!        real(dr) :: DayZero, TShift
!
!
!        ! WatBal file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Water_balance file: '//trim(fname))
!
!        read(FnumTG,*) var 
!        read(FnumTG,*) Time1, areaERL1, areaCPL1, areaSRL1  
!        read(FnumTG,*) Time2, areaERL2, areaCPL2, areaSRL2
!        read(FnumTG,*) ValStart 
!        read(FnumTG,*) ValEnd
!        read(FnumTG,*) DayZero
!       
!
!        FNameNrmlDrainDown=trim(fname)//'.nrmlzd_draindown.dat'
!        call OpenAscii(FNumNrmlDrainDown,FNameNrmlDrainDown)
!        call Msg( 'Draindown working file: '//FNameNrmlDrainDown(:len_trim(FNameNrmlDrainDown)))
!        
!        FNameNrmlDrainDownCSV=trim(fname)//'.draindown.csv'
!        call OpenAscii(FNumNrmlDrainDownCSV,FNameNrmlDrainDownCSV)
!        call Msg( 'Draindown CSV file: '//FNameNrmlDrainDownCSV(:len_trim(FNameNrmlDrainDownCSV)))
!
!        read(FNum,'(a)',iostat=status) TmpSTR  ! title
!        read(FNum,'(a)',iostat=status) TmpSTR  ! variables
!        ! If last character is a comma, change it to a blank (water_balance.dat files have an extra comma)
!        if(TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))==',') TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))=' '
!        nvar=CountChars(COMMA,TmpSTR(index(TmpSTR,'='):)) ! subract 1 for time (1st column)
!        allocate(MnthlyIntegratedFlow(nvar), VarFlow(nvar), stat=ialloc)
!	    call AllocChk(ialloc,'allocate MnthlyIntegratedFlow arrays')
!	    MnthlyIntegratedFlow(:)=0.0d0
!	    VarFlow(:)=0.0d0
!
!        read(FNum,'(a)',iostat=status) TmpSTR  ! zone
!        do 
!            read(FNum,'(a)',iostat=status) TmpSTR
!	        if(status /= 0) exit
!
!	        read(TmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!            if(Simtime >= Time1) exit
!        end do
!            
!        ReadStart= VarFlow(nvar)
!
!        do 
!            read(FNum,'(a)',iostat=status) TmpSTR
!	        if(status /= 0) exit
!
!	        read(TmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!            if(Simtime >= Time2) exit
!        end do
!        ReadEnd= VarFlow(nvar)
!
!        ! Now read watbal file and write normalized seepage rates
!        rewind(FNum)
!        read(FNum,'(a)',iostat=status) TmpSTR  ! title
!        read(FNum,'(a)',iostat=status) TmpSTR  ! variables
!        read(FNum,'(a)',iostat=status) TmpSTR  ! zone
!        do 
!            read(FNum,'(a)',iostat=status) TmpSTR
!	        if(status /= 0) exit
!
!	        read(TmpSTR,*) SimTime
!            if(Simtime >= Time1) then
!                backspace(FNum)
!                continue
!                exit
!            end if    
!        end do
!
!        write(FNumNrmlDrainDown,'(a)') 'variables="Time","Seepage","Slope","Dim_Seepage","Dim_Time","LPerS","TShift","ToERL","ToCPL","ToSRL"'
!        write(FNumNrmlDrainDown,'(a)') 'zone T="Teck:Draindown"'
!
!        write(FNumNrmlDrainDownCSV,'(a)') 'Time,ToERL,ToCPL,ToSRL'
!        
!        TShift=DayZero-Simtime
!   
!        do 
!            read(FNum,'(a)',iostat=status) TmpSTR
!	        if(status /= 0) exit
!
!	        read(TmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!            Slope=(VarFlow(nvar)-LastSeepage)/(SimTime-LastTime)           
!            NrmlSeepage=(VarFlow(nvar)-Readend)/(ReadStart-ReadEnd)
!            TFactor=1.0d0-(Time2-SimTime)/(Time2-Time1)
!            LPerS=ValEnd+(ValStart-Valend)*NrmlSeepage
!            ToERL=areaERL1+(areaERL2-areaERL1)*TFactor
!            ToCPL=areaCPL1+(areaCPL2-areaCPL1)*TFactor
!            ToSRL=areaSRL1+(areaSRL2-areaSRL1)*TFactor
!            
!            If(Slope>=0.0 .and. Slope <= 25) then
!                write(FNumNrmlDrainDown,'(12f15.5)') SimTime,VarFlow(nvar),Slope,NrmlSeepage,TFactor, LPerS, SimTime+TShift, LPerS*ToERL/100.0d0,  LPerS*ToCPL/100.0d0,  LPerS*ToSRL/100.0d0   
!                
!                
!                write(FNumNrmlDrainDownCSV,'(f15.5,3(a,f15.5))') SimTime+TShift,",", LPerS*ToERL/100.0d0,",",  LPerS*ToCPL/100.0d0,",",  LPerS*ToSRL/100.0d0 
!                
!                
!                
!            end if
!            
!            LastTime=SimTime
!            LastSeepage=VarFlow(nvar)
!
!            
!            if(Simtime >= Time2) exit
!        end do
!
!        
!        call FreeUnit(FNum)
!        call FreeUnit(FNumNrmlDrainDown)
!        call FreeUnit(FNumNrmlDrainDownCSV)
!
!
!    end subroutine HGS_TeckRatesWatBalFile
!    
!    !------------------------------------------------------------------------
!    ! Integrate all flows in a water balance file
!    subroutine HGS_IntegrateFlowWatBalFile(FnumTG)
!        implicit none
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FNumFlow
!        character(MAXLBL) :: FNameFlow
!        integer :: FNumFlow2
!        character(MAXLBL) :: FNameFlow2
!                
!        integer :: i, nvar
!        
!
!        real(dr) :: SimTimeLast
!        real(dr) :: SimTime
!        real(dr) :: DeltaTime
!        real(dr), allocatable :: IntegratedFlow(:),VarFlow(:)   
!        
!        real(dr) :: ScaleFactor     
!        
!        ! WatBal file 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Water_balance file: '//trim(fname))
!
!        read(FnumTG,*) ScaleFactor  ! e.g.  = 1 for m3/d if output is in kilogram-metre-day
!        
!
!        FNameFlow=trim(fname)//'.Flow.csv'
!        call OpenAscii(FNumFlow,FNameFlow)
!        call Msg( 'CSV output file: '//FNameFlow(:len_trim(FNameFlow)))
!        
!        FNameFlow2=trim(fname)//'.Flow.dat'
!        call OpenAscii(FNumFlow2,FNameFlow2)
!        call Msg( 'Tecplot output file: '//FNameFlow2(:len_trim(FNameFlow2)))
!
!        read(FNum,'(a)',iostat=status) TmpSTR  ! title
!        write(FNumFlow2,'(a)') trim(TmpSTR)
!        
!        read(FNum,'(a)',iostat=status) TmpSTR  ! variables
!        write(FNumFlow2,'(a)') trim(TmpSTR)
!        
!        ! If last character is a comma, chang it to a blank (water_balance.dat files have an extra comma)
!        if(TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))==',') TmpSTR(len_trim(TmpSTR):len_trim(TmpSTR))=' '
!        write(FNumFlow,'(a)') TmpSTR(index(TmpSTR,'=')+1:) ! write variable names to csv
!        nvar=CountChars(COMMA,TmpSTR(index(TmpSTR,'='):)) ! subract 1 for time (1st column)
!        allocate(IntegratedFlow(nvar), VarFlow(nvar), stat=ialloc)
!	    call AllocChk(ialloc,'allocate IntegratedFlow arrays')
!	    IntegratedFlow(:)=0.0d0
!	    VarFlow(:)=0.0d0
!
!        read(FNum,'(a)',iostat=status) LongTmpSTR  ! zone
!        
!        
!        read(FNum,'(a)',iostat=status) LongTmpSTR
!        read(LongTmpSTR,*) SimTimeLast,(VarFlow(i),i=1,nvar)
!
!        do 
!            read(FNum,'(a)',iostat=status) LongTmpSTR
!	        if(status /= 0) exit
!
!	        read(LongTmpSTR,*) SimTime,(VarFlow(i),i=1,nvar)
!            DeltaTime=SimTime-SimTimeLast 
!            if(DeltaTime > 0.0d0) then  ! sum flow contribution
!                do i=1,nvar
!                    IntegratedFlow(i)= IntegratedFlow(i)+VarFlow(i)* DeltaTime 
!                end do
!            end if
!
!            SimTimeLast=SimTime 
!
!            
!        end do
!
!        write(FNumFlow,'(50(a1,f20.3))') (',',IntegratedFlow(i)*ScaleFactor,i=1,nvar)
!        write(FNumFlow2,'(50(a1,f20.3))')(',',IntegratedFlow(i)*ScaleFactor,i=1,nvar)
!
!        continue
!	        
!
!        call FreeUnit(FNum)
!        call FreeUnit(FNumFlow)
!
!
!    end subroutine HGS_IntegrateFlowWatBalFile
!    
!    !------------------------------------------------------------------------
!    subroutine HGS_ConvertObsWellToPoint(FnumTG)
!        implicit none
!         
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumPt
!        character(MAXLBL) :: FNamePt
!
!        character(MAXLBL) :: line
!        character(MAXLBL) :: linePt
!                
!       
!        real(dr) :: ptTime
!
!        
!        ! tecplot output file 
!        do
!            read(FnumTG,'(a)') FNAME
!            if(index(FNAME,'end') > 0 ) exit
!            call OpenAscii(FNum,FName)
!            call Msg( 'Obs well file: '//trim(fname))
!            
!            read(FnumTG,'(a)') FNamePt
!            call OpenAscii(FNumPt,FNamePt)
!            call Msg( 'Obs point file: '//FNamePt(:len_trim(FNamePt)))
!
!
!            read(FNum,'(a)') line    ! title line
!            write(FNumPt,'(a)') line
!            
!            read(FNum,'(a)') line     ! variables line
!            l1=index(line,'=')
!            linePt=line(:l1)//'"Time",'//line(l1+1:)
!            write(FNumPt,'(a)') linePt
!            
!            read(FNum,'(a)') line     ! first zone line
!            l1=index(line, 'SOLUTIONTIME=') 
!            write(FNumPt,'(a)') line(:l1-3)
!            
!            ptTime=0.0d0
!            
!            do 
!                read(FNum,'(a)',iostat=status) line
!                if(status /= 0) exit
!                
!                l1=index(line, 'SOLUTIONTIME=') 
!                if(l1 > 0) then
!                    read(line(l1+15:),*) ptTime
!                else
!                    write(FNumPt,'(e12.5,a)') ptTime,line(:len_trim(line))
!                end if
!            
!   
!            end do
!            
!            call FreeUnit(FNum)
!            call FreeUnit(FNumPt)
!
!        end do
!
!
!    end subroutine HGS_ConvertObsWellToPoint
!    !------------------------------------------------------------------------
!    subroutine HGS_InterpolateHydrographs(FnumTG)
!        implicit none
!         
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumPt
!        character(MAXLBL) :: FNamePt
!
!        character(MAXLBL) :: line
!                
!       
!        real(dr) :: TargTime, Timeinterval
!        real(dr) :: PtTime, Surface, Pm, Total
!        real(dr) :: PtTimePrev, SurfacePrev, PmPrev, TotalPrev
!        real(dr) :: IntSurface, IntPm, IntTotal
!        
!        read(FnumTG,*) Timeinterval  ! e.g. value of 1 would be interpolating every day
!        write(TmpSTR,'(f12.5)') Timeinterval
!        do
!            read(FnumTG,'(a)') FNAME
!            if(index(FNAME,'end') > 0 ) exit
!            call OpenAscii(FNum,FName)
!            call Msg( 'Hydrograph file: '//trim(fname))
!            
!            read(FnumTG,'(a)') FNamePt
!            call OpenAscii(FNumPt,FNamePt)
!            call Msg( 'Interped hydrograph file: '//FNamePt(:len_trim(FNamePt)))
!
!
!            read(FNum,'(a)') line    ! title line
!            write(FNumPt,'(a)') line
!            
!            read(FNum,'(a)') line     ! variables line
!            write(FNumPt,'(a)') line
!
!            read(FNum,'(a)') line     ! zone line
!            write(FNumPt,'(a)') line
!
!            read(FNum,*,iostat=status) PtTimePrev, SurfacePrev, PmPrev, TotalPrev
!
!            TargTime=TimeInterval
!            do 
!                read(FNum,*,iostat=status) PtTime, Surface, Pm, Total
!                !write(*,*) PtTime
!                if(status /= 0) exit
!                
!                if(PtTime >= Targtime) then  ! interpolate between this and last values
!                    if(abs(TargTime-35.0) < 1e-3) then
!                        continue
!                    end if
!                    write(*,'(a,3f10.5)') 'interpolate',Targtime, PtTimePrev, PtTime
!                    do while (Targtime<=PtTime)
!                        IntSurface=SurfacePrev+(Targtime-PtTimePrev)/(PtTime-PtTimePrev)*(Surface-SurfacePrev)
!                        IntPm=PmPrev+(Targtime-PtTimePrev)/(PtTime-PtTimePrev)*(Pm-PmPrev)
!                        IntTotal=TotalPrev+(Targtime-PtTimePrev)/(PtTime-PtTimePrev)*(Total-TotalPrev)
!                        write(FNumPt,'(4e18.9)') TargTime,IntSurface,IntPm,IntTotal
!                        Targtime=TargTime+TimeInterval
!                    end do
!                end if
!                PtTimePrev=PtTime
!                SurfacePrev=Surface
!                PmPrev= Pm
!                TotalPrev=Total
!            
!   
!            end do
!            
!            call FreeUnit(FNum)
!            call FreeUnit(FNumPt)
!
!        end do
!
!
!    end subroutine HGS_InterpolateHydrographs
!    !------------------------------------------------------------------------
!    subroutine HGS_ConcatenateTimeSeries(FnumTG)
!    ! Concatenate a set of time series files to one file.  They must  be all of one type e.g. a hydrograph
!    ! Accumulate time as we go i.e. t = t + time in current file
!    ! Each file should have an HGS start time of zero 
!        implicit none
!         
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumPt
!        character(MAXLBL) :: FNamePt
!
!        character(MAXLBL) :: line
!                
!       
!        real(dr) :: TCurr
!        integer :: iRest
!        real(dr) :: PtTime 
!        integer :: nfiles
!        
!        read(FnumTG,'(a)') FNamePt
!        call OpenAscii(FNumPt,FNamePt)
!        call Msg( 'Concatenated hydrograph file: '//FNamePt(:len_trim(FNamePt)))
!
!        read(FnumTG,*) nFiles  ! number of files to concatenate
!        write(TmpSTR,'(i5)') nFiles
!        
!        TCurr=0.0
!        do i=1,nfiles
!            read(FnumTG,'(a)') FNAME
!            if(index(FNAME,'end') > 0 ) exit
!            call OpenAscii(FNum,FName)
!            call Msg( 'Hydrograph file: '//trim(fname))
!
!            
!            read(FNum,'(a)') line    ! title line
!            if(i==1) write(FNumPt,'(a)') line
!            
!            read(FNum,'(a)') line     ! variables line
!            if(i==1)  write(FNumPt,'(a)') line
!
!            read(FNum,'(a)') line     ! zone line
!            write(FNumPt,'(a)') 'zone t="'//trim(fname)//'"'
!
!            do 
!                read(FNum,'(a)',iostat=status) line
!                if(status /= 0) exit
!                
!                line=adjustl(line)
!                read(line,*) PtTime
!                
!                ! Find first blank
!                iRest=index(line," ")
!           
!                write(FNumPt,'(e18.9,a)') TCurr+PtTime,line(iRest:)
!             
!   
!            end do
!            TCurr=Tcurr+PtTime
!           
!            call FreeUnit(FNum)
!
!        end do
!
!        call FreeUnit(FNumPt)
!
!    end subroutine HGS_ConcatenateTimeSeries
!
!
!
!    !------------------------------------------------------------------------
!    subroutine HGS_ZoneBudget2(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!        
!        
!        
!        integer :: i, j, k
!        integer :: ZoneNum, MaxZoneNum
!        integer :: FNumZoneBudget
!        character(MAXLBL) :: FNameZoneBudget
!        integer :: FNumTecplotZoneBudget
!        character(MAXLBL) :: FNameTecplotZoneBudget
!        logical :: ZonesLinked
!        integer :: npcon, npact
!        logical :: TecplotOutput
!        
!        
!        read(FnumTG,'(a)') GrokPfx
!        LGrokPfx=len_trim(GrokPfx)
!        
!        ! Process this zone
!        read(FnumTG,*) ZoneNum
!        write(TmpSTR,'(i4.4)') ZoneNum
!        FNameZoneBudget=GrokPfx(:LGrokPfx)//'o.Zone_'//trim(TMPStr)//'_Budget.ginc'
!        call OpenAscii(FNumZoneBudget,FNameZoneBudget)
!        call date_and_time(DateSTR, TIME = TimeSTR, ZONE = TimezoneSTR)
!        write(FNumZoneBudget,'(a)') '! Created by program TG '//TGVersion//', HGS Zone Budget generator '//DateSTR
!        write(FNumZoneBudget,'(a)') '! From file prefix: '//GrokPfx(:LGrokPfx)
!        write(FNumZoneBudget,'(a,i5)') '! For zone: ',ZoneNum
!        write(FNumZoneBudget,'(a)') '! ***THIS FILE MAY BE OVERWRITTEN so copy and edit'
!
!        read(FnumTG,*) TecplotOutput
!        if(TecplotOutput) then
!            call Msg('Write Zone Budget active and contributing nodes to Tecplot as scatter data')
!            FNameTecplotZoneBudget=GrokPfx(:LGrokPfx)//'o.Zone_'//trim(TMPStr)//'_Budget_nodes.dat'
!            call OpenAscii(FNumTecplotZoneBudget,FNameTecplotZoneBudget)
!            write(FNumTecplotZoneBudget,'(a)') 'TITLE = " Created by program TG '//TGVersion//', HGS Zone Budget generator '//DateSTR//'"'
!            write(FNumTecplotZoneBudget,'(a)') 'TITLE = " For file prefix: '//GrokPfx(:LGrokPfx)//'"'
!            write(FNumTecplotZoneBudget,'(a)') 'VARIABLES = "X","Y","Z"'
!        end if
!        
!        
!
! 
!        hgs.node_is(:)=0
!        MaxZoneNum=0
!        
!        ! For all ZoneNum elements, flag the nodes as chosen
!        do i=1,hgs.mesh.ne
!            if(hgs.iprop(i)>MaxZoneNum) then
!                MaxZoneNum=hgs.iprop(i)
!            end if
!            if(hgs.iprop(i)==ZoneNum) then
!                do j=1,hgs.mesh.nln
!                    call set(hgs.node_is(hgs.mesh.in(j,i)),chosen)
!                end do
!            end if
!        end do
!        
!        write(TmpSTR,'(a,i8)') 'Biggest zone #: ', MaxZoneNum
!        call Msg(trim(TMPStr))
!
!        do k=1,MaxZoneNum
!            if(k==ZoneNum) cycle ! only consider other zones
!            
!            ! Clear active and contributing node sets
!            do i=1,hgs.mesh.nn
!                call clear(hgs.node_is(i),active)
!                call clear(hgs.node_is(i),contributing)
!            end do
!
!            ZonesLinked=.false.
!
!            do i=1,hgs.mesh.ne
!                if(hgs.iprop(i) /= k) cycle  ! only gather info for zone k
!
!                npact=0
!                npcon=0
!		        do j=1,hgs.mesh.nln
!			        if(bcheck(hgs.node_is(hgs.mesh.in(j,i)),chosen)) then
!				        npact=npact+1
!			        else
!				        npcon=npcon+1
!			        end if
!		        end do
!
!		        if(npact > 0 .and. npcon > 0) then ! element contains both chosen and unchosen nodes
!                    ZonesLinked=.true.
!			        do j=1,hgs.mesh.nln
!				        if(bcheck(hgs.node_is(hgs.mesh.in(j,i)),chosen)) then  ! flag as active
!					        call set(hgs.node_is(hgs.mesh.in(j,i)),active)
!				        else   ! flag as contributing
!					        call set(hgs.node_is(hgs.mesh.in(j,i)),contributing)
!				        end if
!			        end do
!		        end if
!            end do
!            
!            if(ZonesLinked) then ! write the information for computing the zone budget
!                write(FNumZoneBudget,'(a)') ' '
!                write(FNumZoneBudget,'(a,i5)') '!------------------------ Flux crossing slice data for zone',k
!                ! Create active node list file
!                write(TmpSTR,'(i4.4,a,i4.4)') Zonenum,'_',k
!                FnameActive=GrokPfx(:LGrokPfx)//'o.nlist.'//trim(TMPStr)//'.active'
!                
!                write(FNumTecplotZoneBudget,'(a)') 'ZONE T= "Zone_'//trim(TMPStr)//' active"'
!                
!                write(FNumZoneBudget,'(a)') 'clear chosen nodes'
!                write(FNumZoneBudget,'(a)') 'choose nodes list'
!                write(FNumZoneBudget,'(a)') FnameActive(:len_trim(FnameActive))
!                call OpenAscii(FnumActive,FnameActive)
!                do i=1,hgs.mesh.nn
!                    if(bcheck(hgs.node_is(i),active)) then
!                        write(FnumActive,*) i
!                        write(FNumTecplotZoneBudget,*) hgs.mesh.x(i),hgs.mesh.y(i),hgs.mesh.z(i)
!                    end if
!                end do
!                call freeunit(FnumActive)
!                write(FNumZoneBudget,'(a)') 'slice flux output nodes from chosen'
!                write(FNumZoneBudget,'(a)') 'Zones_'//TmpSTR
!               
!                ! Create contributing node list file
!                FnameContrib=GrokPfx(:LGrokPfx)//'o.nlist.'//trim(TMPStr)//'.contributing'
!
!                write(FNumTecplotZoneBudget,'(a)') 'ZONE T= "Zone_'//trim(TMPStr)//' contributing"'
!                
!                write(FNumZoneBudget,'(a)') 'clear chosen nodes'
!                write(FNumZoneBudget,'(a)') 'choose nodes list'
!                write(FNumZoneBudget,'(a)') FnameContrib(:len_trim(FnameContrib))
!                call OpenAscii(FnumContrib,FnameContrib)
!                do i=1,hgs.mesh.nn
!                    if(bcheck(hgs.node_is(i),contributing)) then
!                        write(FnumContrib,*) i
!                        write(FNumTecplotZoneBudget,*) hgs.mesh.x(i),hgs.mesh.y(i),hgs.mesh.z(i)
!                    end if
!                end do
!                call freeunit(FnumContrib)
!                write(FNumZoneBudget,'(a)') 'slice flux contributing nodes from chosen'
!            end if
!
!            
!        end do
!
!        
!        continue
!        
!    end subroutine HGS_ZoneBudget2
!    !------------------------------------------------------------------------
!    subroutine HGS_AdjustTopGbNprop(FnumTG, hgs) ! use a grid builder nprop file to adjust the top elevations 
!        ! Originally used to build beaver dams for madawaska :)
!        implicit none
!        
!        integer :: i
!        character(80)       :: dummy 
!        
!        type (HGSProject) hgs
!
!        integer :: FnumGbNprop
!        character(MAXLBL) :: FnameGbNprop
!
!        integer :: FnumTG
!        
!        integer :: nd3d
!        
!        read(FnumTG,'(a)') FnameGbNprop
!     
!        call OpenBinary(FnumGbNprop,FnameGbNprop)
!        read(FnumGbNprop) dummy 
!        
!        allocate(hgs.nprop(hgs.nn2d), stat=ialloc)
!	    call AllocChk(ialloc,'allocate hgs nprop arrays')
!	    hgs.nprop(:)=0.0d0
!
!
!        read(FnumGbNprop,iostat=status) (hgs.nprop(i),i=1,hgs.nn2d)  
!	    if(status /= 0) then 
!	        write(ErrStr,'(a)') 'File: '//FnameGbNprop
!	        write(ErrStr,'(a)') 'Error reading file: '//FnameGbNprop
!		    call ErrMsg(ErrStr)
!	    end if
!
!        call freeunit(FnumGbNprop)
!        
!        ! Now check elevation
!        do i=1,hgs.nn2d
!            if(hgs.nprop(i) > 0.0d0) then
!                nd3d=hgs.mesh.nn-hgs.nn2d+i
!                write(TmpSTR,*) hgs.mesh.z(nd3d),'+',hgs.nprop(i),'=',hgs.mesh.z(nd3d)+hgs.nprop(i)
!                call Msg(TmpSTR)
!                hgs.mesh.z(nd3d)=hgs.mesh.z(nd3d)+hgs.nprop(i)
!            end if
!        end do
!
!    end subroutine HGS_AdjustTopGbNprop
!    !------------------------------------------------------------------------
!    subroutine HGS_ReadGbNprop(FnumTG, hgs) ! read a grid builder nprop file
!        implicit none
!        
!        integer :: i
!        character(80)       :: dummy 
!        
!        type (HGSProject) hgs
!
!        integer :: FnumGbNprop
!        character(MAXLBL) :: FnameGbNprop
!
!        integer :: FnumTG
!        
!        
!        read(FnumTG,'(a)') FnameGbNprop
!     
!        call OpenBinary(FnumGbNprop,FnameGbNprop)
!        read(FnumGbNprop) dummy 
!        
!        allocate(hgs.nprop(hgs.nn2d), stat=ialloc)
!	    call AllocChk(ialloc,'allocate hgs nprop arrays')
!	    hgs.nprop(:)=0.0d0
!
!
!        read(FnumGbNprop,iostat=status) (hgs.nprop(i),i=1,hgs.nn2d)  
!	    if(status /= 0) then 
!	        write(ErrStr,'(a)') 'File: '//FnameGbNprop
!	        write(ErrStr,'(a)') 'Error reading file: '//FnameGbNprop
!		    call ErrMsg(ErrStr)
!	    end if
!
!        call freeunit(FnumGbNprop)
!
!    end subroutine HGS_ReadGbNprop
!
!    !------------------------------------------------------------------------
!    subroutine hgsayerHeadsToSurfer(FnumTG, hgs) ! use a head output file and write the contents out layer by layer 
!        ! Originally used to write  for madawaska :)
!        implicit none
!        
!        integer :: i
!        character(80)       :: dummy 
!        
!        type (HGSProject) hgs
!
!        integer :: FnumHGSHeads
!        character(MAXLBL) :: FnameHGSHeads
!
!        integer :: FnumTG
!        
!        integer :: nd3d
!        
!        read(FnumTG,'(a)') FnameHGSHeads
!     
!        call OpenBinary(FnumHGSHeads,FnameHGSHeads)
!        read(FnumHGSHeads) dummy 
!        
!        allocate(hgs.nprop(hgs.nn2d), stat=ialloc)
!	    call AllocChk(ialloc,'allocate hgs nprop arrays')
!	    hgs.nprop(:)=0.0d0
!
!
!        read(FnumHGSHeads,iostat=status) (hgs.nprop(i),i=1,hgs.nn2d)  
!	    if(status /= 0) then 
!	        write(ErrStr,'(a)') 'File: '//FnameHGSHeads
!	        write(ErrStr,'(a)') 'Error reading file: '//FnameHGSHeads
!		    call ErrMsg(ErrStr)
!	    end if
!
!        call freeunit(FnumHGSHeads)
!        
!        ! Now check elevation
!        do i=1,hgs.nn2d
!            if(hgs.nprop(i) > 0.0d0) then
!                nd3d=hgs.mesh.nn-hgs.nn2d+i
!                write(TmpSTR,*) hgs.mesh.z(nd3d),'+',hgs.nprop(i),'=',hgs.mesh.z(nd3d)+hgs.nprop(i)
!                call Msg(TmpSTR)
!                hgs.mesh.z(nd3d)=hgs.mesh.z(nd3d)+hgs.nprop(i)
!            end if
!        end do
!
!    end subroutine hgsayerHeadsToSurfer
!
!    subroutine HGS_WriteNodalOutputToFeflowDacFormat(FnumTG, hgs)
!        implicit none
!        type (HGSProject) hgs
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!                
!        integer :: i
!        real(dr) :: stime
!
!        
!        ! Feflow Dac file format 
!        read(FnumTG,'(a)') FNAME
!        call OpenAscii(FNum,trim(fname))
!        call Msg( 'Feflow Dac output file: '//trim(fname))
!        
!        read(hgs.message,*)  stime   
!
!
!        write(FNum,'(a1,i5,a1,e13.6)') '$',0,',',stime
!        write(FNum,'(12(e21.14,a))') (hgs.sat(HGS2Feflow_Nnum(i,hgs)),',',i=1,hgs.mesh.nn)
!        call FreeUnit(FNum)
!
!
!    end subroutine HGS_WriteNodalOutputToFeflowDacFormat
!    !------------------------------------------------------------------------
!   subroutine HGS_OLfExchangeFluxByPolyGon(FnumTG,hgs)
!        ! hgs is the HGS porous media data set that was loaded by conventional HGS instruction "read components"
!        ! we will create and load another data set (hgs2) to compare it to.
!        implicit none
!        
!        integer :: i,j,k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!        real(dr) :: bxr(4), byr(4)
!
!        type (HGSProject) hgs
!        type (HGSProject) hgs2
!        
!        real, allocatable :: elist(:)
!        
!        integer :: elem3d, elem3d_2
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'K comparison output file: '//FName)
!
!        
!        call HGS_GetMeshComponents(FnumTG, hgs2)
!        call HGS_ReadElemK(FnumTG, hgs2)
!        
!        continue
!        
!        if(hgs.mesh.ne==hgs2.mesh.ne) then ! they have the same number of elements.  Compare them without regard to geometry.....!!!!
!
!            write(FNum,'(a)') 'variables = "x", "y", "z","Ratio Kx1/Kx2","Ratio Ky1/Ky2","Ratio Kz1/Kz2"'
!            
!            do i=1,hgs.mesh.ne
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                write(FNum,'(6e20.12)') xe, ye, ze,hgs.ElemKx(i)/hgs2.ElemKx(i),hgs.ElemKy(i)/hgs2.ElemKy(i),hgs.ElemKz(i)/hgs2.ElemKz(i) 
!            end do
!            
!        else
!            ! assuming a layered structure of 2D meshes
!            ! loop over ne2d and form a list of corresponding element numbers from the second mesh.
!            allocate(elist(hgs.ne2d))
!            loop1: do i=1,hgs.ne2d
!                call find_elem_centroid(hgs.mesh,i,xe,ye,ze)
!                call PercentDone(i,hgs.ne2d)
!
!                    
! 			    do  j=1,hgs2.ne2d   
!                    do k=1,3
!                        bxr(k)=hgs.mesh.x(hgs.mesh.in(k,j))
!                        byr(k)=hgs.mesh.y(hgs.mesh.in(k,j))
!                    end do
!                    bxr(4)=bxr(1)
!                    byr(4)=byr(1)
!
!				    if(in_poly(4,bxr,byr,xe,ye)) then
!					    elist(i)=j 
!                        cycle loop1
!				    end if 
!			    end do 
!
!            
!            end do loop1
!            ! loop over layers and write the differences
!            ! elist(i) contains the corresponding 2d element number from the second hgs domain
!            
!            write(FNum,'(a)') 'variables = "x", "y","Ratio Kx1/Kx2","Ratio Ky1/Ky2","Ratio Kz1/Kz2"'
!
!            do i=1,hgs.nsheet-1   ! loop over the layers
!                write(TMPStr,'(a,i3,a,i3,a)') 'zone  t=" Feflow Layer ',hgs.nsheet-i,' (HGS Layer ',i,')"'
!                write(FNum,'(a)') trim(TMPStr)
!                do j=1, hgs.ne2d
!                    call find_elem_centroid(hgs.mesh,j,xe,ye,ze)
!                    elem3d=(i-1)*hgs.ne2d+j
!                    elem3d_2=(i-1)*hgs2.ne2d+elist(j)
!                    write(FNum,'(5e20.12)') xe, ye, hgs.ElemKx(elem3d)/hgs2.ElemKx(elem3d_2),hgs.ElemKy(elem3d)/hgs2.ElemKy(elem3d_2),hgs.ElemKz(elem3d)/hgs2.ElemKz(elem3d_2) 
!                end do
!   
!            end do
!            
!            continue
!        end if
!
!        call FreeUnit(FNum)
!              
!
!    end subroutine HGS_OLfExchangeFluxByPolyGon
!
!
!  !----------------------------------------------------------------------
!    double precision function fluid_viscosity_temperature(temperature_deg)
!	    !.....................................................................
!	    !
!	    ! AUTHOR		Thomas Graf
!	    !
!	    ! AFFILIATION	Georg-August University Goettingen
!	    !
!	    ! DATE			December 3, 2008
!	    !
!	    ! DESCRIPTION	Calculates the fluid viscosity from temperature
!	    !
!	    ! LITERATURE	Molson et al. (1992, WRR)
!	    !				Pawlowski (1991)
!	    !				JSME (1968)
!	    !
!	    ! INPUT			temperature in centigrade			--
!	    !
!	    ! OUTPUT		fluid viscosity						kg m-1 sec-1
!	    !
!	    !.....................................................................
!
!        implicit none
!
!	    real(dr) :: T,T_C,temperature_deg
!	    real(dr) :: T_th1,T_th2
!	    real(dr) :: power
!	    real(dr) :: A,B,C
!
!	    !!! WATCH OUT FOR UNITS !!!
!
!	    ! relative temperature T_C in centigrade
!	    T_C = temperature_deg
!
!	    ! absolute temperature in Kelvin
!	    T = T_C + 273.15d0
!
!	    ! threshold temperatures in centigrade
!	    T_th1 = 40.0d0
!	    T_th2 = 100.0d0
!
!	    if(T_C.le.T_th1) then
!		    ! use Molson's model
!
!		    ! set constants
!		    A = 1.787d-3
!		    B = -3.288d-2
!		    C = 1.962d-4
!
!		    fluid_viscosity_temperature = A * exp((B+C*T_C) * T_C)
!		    return
!
!	    elseif(T_C.gt.T_th1 .and. T_C.le.T_th2) then
!		    ! use Pawlowski's model
!
!		    ! set constants
!		    A = 1.5512d-2
!		    B = -1.572d0
!		    C = 1.0d0 + A * (T_C-20.0d0)
!
!		    fluid_viscosity_temperature = 1.0d-3 * power(C,B)
!		    return
!
!	    elseif(T_C.gt.T_th2) then
!		    ! use JSME model
!
!		    ! set constants
!		    A = 241.4d-3
!		    B = 247.8d0
!		    C = B / (T-140.0d0)
!
!		    fluid_viscosity_temperature = A * 1.0d-4 * power(10.0d0,C)
!		    return
!
!	    end if
!
!    end function fluid_viscosity_temperature
!    
!    !------------------------------------------------------------------------
!    subroutine read_var8(np,fname,var,message)
!	     
!	    implicit none
!        integer :: itmp
!        character*80 :: message
!
!	    integer :: j, np
!	    character(*) :: fname
!	    real(dr) :: var(np)
!
!	    call getunit(itmp)
!	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
!	    !if(status /= 0) then
!		   ! call ErrMsg('FILE ERROR: '//fname)
!		   ! stop
!	    !end if
!	    read(itmp) message
!	    read(itmp) (var(j),j=1,np)
!	    call freeunit(itmp)
!
!    end subroutine read_var8
!    !------------------------------------------------------------------------
!    subroutine write_var8(np,fname,var,message)
!	     
!	    implicit none
!        integer :: itmp
!        character*80 :: message
!
!	    integer :: j, np
!	    character(*) :: fname
!	    real(dr) :: var(np)
!
!	    call getunit(itmp)
!	    open(itmp,file=fname,status='unknown',action = 'write',form='unformatted')
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!	    end if
!	    write(itmp) message
!	    write(itmp) (var(j),j=1,np)
!	    call freeunit(itmp)
!
!    end subroutine write_var8
!    !------------------------------------------------------------------------
!    subroutine read_var4(np,fname,var,message)
!	     
!	    implicit none
!        integer :: itmp, status
!        character*80 :: message
!
!	    integer :: j, np
!	    character(*) :: fname
!	    real :: var(np)
!
!	    call getunit(itmp)
!	    open(itmp,file=fname,status='old',action = 'read',form='unformatted')
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!	    end if
!	    read(itmp) message
!	    read(itmp) (var(j),j=1,np)
!	    call freeunit(itmp)
!
!    end subroutine read_var4
!    !------------------------------------------------------------------------
!    subroutine TecplotVarAdd(varname,gnvar,vnvar,gvarlist)
!	    implicit none
!    	
!	    integer :: gnvar, vnvar
!	    character(*) :: varname
!	    character(512) :: gvarlist
!
!	    if(gnvar==0) then
!	        gvarlist=trim(varname)
!	    else
!	        gvarlist=trim(gvarList)//' '//trim(varname)
!	    end if
!	    gnvar=gnvar+1
!	    vnvar=gnvar
!    end subroutine TecplotVarAdd
!    
!    !integer function HGS2Feflow_Enum(ii)
!    !    implicit none
!    !    integer :: ii, j, i_layer
!    !
!    !    
!    !    j=mod(ii,ne2d)
!    !    if(j==0) j=ne2d
!    !
!    !    i_layer=(ii-j)/ne2d+1
!    !    HGS2Feflow_Enum=(n_layers-i_layer)*ne2d+j
!    !
!    !    if(HGS2Feflow_Enum==1) then
!    !        continue
!    !    end if
!    !
!    !end function HGS2Feflow_Enum
!
!    integer function HGS2Feflow_Nnum(ii,hgs)
!	    implicit none
!        type (HGSProject) hgs
!        integer :: ii, j, i_sheet
!
!             !           hgs.nsheet=hgs.nz
!	            !read(itmp) hgs.ne2d  ! for ET stuff
!
!        j=mod(ii,hgs.nn2d)
!        if(j==0) j=hgs.nn2d
!    
!        i_sheet=(ii-j)/hgs.nn2d+1
!        HGS2Feflow_Nnum=(hgs.nsheet-i_sheet)*hgs.nn2d+j
!    
!    end function HGS2Feflow_Nnum
!
!    !------------------------------------------------------------------------
!   subroutine HGS_KvsDepthCrossSection(FnumTG,hgs)
!        ! For writing K vs depth for a cross-sectional HGS model with y vertical
!        ! - read the mesh (i.e porous media component)
!        ! - read the list of top nodes then sort by X
!        implicit none
!        
!        integer :: i, j, k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!
!        type (HGSProject) hgs
!        
!        integer :: iNode, iElem
!        
!        real(dr), allocatable :: DepthTbl(:), KxxTbl(:), KyyTbl(:), KzzTbl(:)
!        real(dr) :: DepthElem
!        integer :: nTbl
!        
!        real(dr) :: xi_temp(hgs.mesh.ne)
!        real(dr) :: yi_temp(hgs.mesh.ne)
!        integer :: nx
!        integer :: indx_col(hgs.mesh.ne)
!        
!        real(dr) :: x1, x2
!        real(dr) :: y1, y2
!        real(dr) :: yGround
!       
!        xi_temp(:)=0.0
!        yi_temp(:)=0.0
!
!        read(FnumTG,*) nTbl
!        allocate(DepthTbl(nTbl), KxxTbl(nTbl), KyyTbl(nTbl), KzzTbl(nTbl))
!        do i=1,nTbl
!            read(FnumTG,*) DepthTbl(i), KxxTbl(i), KyyTbl(i), KzzTbl(i)
!            KxxTbl(i)=log10(KxxTbl(i))
!            KyyTbl(i)=log10(KyyTbl(i))
!            KzzTbl(i)=log10(KzzTbl(i))
!        end do
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Feflow elemental K file: '//FName)
!
!        if(.not. allocated(Kxx)) allocate(Kxx(hgs.mesh.ne),Kyy(hgs.mesh.ne),Kzz(hgs.mesh.ne))
!
!        do i=1,hgs.mesh.ne
!			read(FNum,'(i10,3e20.12)') j,kxx(j),kyy(j),kzz(j)
!		end do
!
!
!        continue
!       
!        ! sort most recent node list hgs.nNodeList
!        nx=hgs.NodeListSize(hgs.nNodeList)
!        do i=1,nx
!            iNode=hgs.NodeList(hgs.nNodeList,i)
!            xi_temp(i)=hgs.mesh.x(iNode)
!            yi_temp(i)=hgs.mesh.y(iNode)
!        end do
!        call indexx3(nx,xi_temp,indx_col) 
!
!        
!       
!        ! loop over elements in most recent list hgs.nElemList
!        do i=1,hgs.ElementListSize(hgs.nElementList)
!            iElem=hgs.ElementList(hgs.nElementList,i)
!            call find_elem_centroid(hgs.mesh,iElem,xe,ye,ze)
!            do j=1,nx-1
!                x1=xi_temp(indx_col(j))
!                x2=xi_temp(indx_col(j+1))
!                if(xe>=x1 .and. xe<=x2) then  ! use 
!                    y1=yi_temp(indx_col(j))
!                    y2=yi_temp(indx_col(j+1))
!                    yGround=y1+(xe-x1)/(x2-x1)*(y2-y1)
!                    DepthElem=yGround-ye
!                    do k=1,nTbl-1
!                        if(DepthElem>=DepthTbl(k) .and. DepthElem<=DepthTbl(k+1)) then  ! interpolate k and write
!                            Kxx(ielem)=10**(KxxTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KxxTbl(k+1)-KxxTbl(k)))
!                            Kyy(ielem)=10**(KyyTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KyyTbl(k+1)-KyyTbl(k)))
!                            Kzz(ielem)=10**(KzzTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KzzTbl(k+1)-KzzTbl(k)))
!                        end if
!                    end do
!                    
!                    continue
!                end if
!            end do
!        
!        end do
!        
!        rewind(FNum)
!        do j=1,hgs.mesh.ne
!			write(FNum,'(i10,3e20.12)') j,kxx(j),kyy(j),kzz(j)
!		end do
!
!        call FreeUnit(FNum)
!              
!
!   end subroutine HGS_KvsDepthCrossSection
!    !------------------------------------------------------------------------
!   subroutine HGS_KvsDepthCrossSectionV2(FnumTG,hgs)
!        ! For writing K vs depth for a cross-sectional HGS model with y vertical
!        ! - read the mesh (i.e porous media component)
!        ! - read the list of top nodes then sort by X
!        implicit none
!        
!        integer :: i, j, k
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!	    real(dr) :: xe, ye, ze  ! centroid
!
!        type (HGSProject) hgs
!        
!        integer :: iNode, iElem
!        
!        real(dr), allocatable :: DepthTbl(:), KxxTbl(:), KyyTbl(:), KzzTbl(:)
!        real(dr), allocatable :: KxxTemp(:), KyyTemp(:), KzzTemp(:)
!        real(dr) :: DepthElem
!        integer :: nTbl
!        
!        real(dr) :: xi_temp(hgs.mesh.ne)
!        real(dr) :: yi_temp(hgs.mesh.ne)
!        integer :: nx
!        integer :: indx_col(hgs.mesh.ne)
!        
!        real(dr) :: x1, x2
!        real(dr) :: y1, y2
!        real(dr) :: yGround
!       
!        xi_temp(:)=0.0
!        yi_temp(:)=0.0
!
!        read(FnumTG,*) nTbl
!        allocate(DepthTbl(nTbl), KxxTbl(nTbl), KyyTbl(nTbl), KzzTbl(nTbl))
!        do i=1,nTbl
!            read(FnumTG,*) DepthTbl(i), KxxTbl(i), KyyTbl(i), KzzTbl(i)
!            KxxTbl(i)=log10(KxxTbl(i))
!            KyyTbl(i)=log10(KyyTbl(i))
!            KzzTbl(i)=log10(KzzTbl(i))
!        end do
!           
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Feflow elemental K file: '//FName)
!
!       
!        ! sort most recent node list hgs.nNodeList
!        nx=hgs.NodeListSize(hgs.nNodeList)
!        do i=1,nx
!            iNode=hgs.NodeList(hgs.nNodeList,i)
!            xi_temp(i)=hgs.mesh.x(iNode)
!            yi_temp(i)=hgs.mesh.y(iNode)
!        end do
!        call indexx3(nx,xi_temp,indx_col) 
!
!        allocate(KxxTemp(hgs.mesh.ne),KyyTemp(hgs.mesh.ne),KzzTemp(hgs.mesh.ne))
!        KxxTemp(:)=-999.  
!        KyyTemp(:)=-999.
!        KzzTemp(:)=-999.
!       
!        ! loop over elements in most recent list hgs.nElemList
!        do i=1,hgs.ElementListSize(hgs.nElementList)
!            iElem=hgs.ElementList(hgs.nElementList,i)
!            if(ielem==8999) then 
!                continue
!            end if
!            call find_elem_centroid(hgs.mesh,iElem,xe,ye,ze)
!            do j=1,nx-1
!                x1=xi_temp(indx_col(j))
!                x2=xi_temp(indx_col(j+1))
!                if(xe>=x1 .and. xe<=x2) then  ! use 
!                    y1=yi_temp(indx_col(j))
!                    y2=yi_temp(indx_col(j+1))
!                    yGround=y1+(xe-x1)/(x2-x1)*(y2-y1)
!                    DepthElem=yGround-ye
!                    do k=1,nTbl-1
!                        if(DepthElem>=DepthTbl(k) .and. DepthElem<=DepthTbl(k+1)) then  ! interpolate k and write
!                            KxxTemp(ielem)=10**(KxxTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KxxTbl(k+1)-KxxTbl(k)))
!                            if(kxxTemp(ielem) < 0.0) then
!                                continue
!                            end if
!                            KyyTemp(ielem)=10**(KyyTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KyyTbl(k+1)-KyyTbl(k)))
!                            KzzTemp(ielem)=10**(KzzTbl(k)+(DepthElem-DepthTbl(k))/(DepthTbl(k+1)-DepthTbl(k))*(KzzTbl(k+1)-KzzTbl(k)))
!                        end if
!                    end do
!                    
!                    continue
!                end if
!            end do
!        
!        end do
!        
!        rewind(FNum)
!        do i=1,hgs.ElementListSize(hgs.nElementList)
!            iElem=hgs.ElementList(hgs.nElementList,i)
!			if(KxxTemp(iElem)/=-999.0) then ! only write if Kxx has been updated (depends on element centroid and k table in tg file)
!                write(FNum,'(i10,3e20.12)') iElem,KxxTemp(iElem),KyyTemp(iElem),KzzTemp(iElem)
!            end if
!		end do
!
!        call FreeUnit(FNum)
!
!        deallocate(KxxTemp,KyyTemp,KzzTemp)
!
!
!   end subroutine HGS_KvsDepthCrossSectionV2
!    !------------------------------------------------------------------------
!   subroutine HGS_RechargeCrossSection(FnumTG,hgs)
!        ! For writing recharge instructions for a cross-sectional HGS model with y vertical
!        ! - read the mesh (i.e porous media component)
!        ! - read the list of top nodes then sort by X
!        ! - read the recharge csv file 
!        !   - header
!        !   - xfrom, xto, recharge rate
!        ! - read a recharge rate multiplier
!   
!        implicit none
!        
!        integer :: i, j
!        integer :: FnumTG
!        
!        integer :: FnumRecharge
!
!        type (HGSProject) hgs
!        
!        integer :: iNode
!        
!        real(dr), allocatable :: xTbl(:),  RechargeTbl(:)
!        real(dr) :: ConversionFactor
!        integer :: nTbl
!        
!        real(dr) :: xi_temp(hgs.mesh.ne)
!        real(dr) :: yi_temp(hgs.mesh.ne)
!        integer :: nx
!        integer :: indx_col(hgs.mesh.ne)
!
!        real(dr) :: xStart, xEnd
!        real(dr) :: Frac
!        integer :: iStart, iEnd
!   
!        
!        real(dr) :: x1, x2
!       
!        xi_temp(:)=0.0
!        yi_temp(:)=0.0
!
!        read(FnumTG,*) ConversionFactor
!        write(TmpSTR,'(e20.5)') ConversionFactor
!        call Msg('Recharge conversion facor '//trim(TmpSTR))
!        read(FnumTG,*) nTbl
!        allocate(xTbl(nTbl), rechargeTbl(nTbl))
!        do i=1,nTbl
!            read(FnumTG,*) xTbl(i), RechargeTbl(i)
!        end do
!        
!        
!           
!
!        continue
!       
!        ! sort most recent node list hgs.nNodeList
!        nx=hgs.NodeListSize(hgs.nNodeList)
!        do i=1,nx
!            iNode=hgs.NodeList(hgs.nNodeList,i)
!            xi_temp(i)=hgs.mesh.x(iNode)
!        end do
!        call indexx3(nx,xi_temp,indx_col) 
!
!        if(.not. allocated(hgs.nchosen)) allocate(hgs.nchosen(hgs.mesh.nn))
!        
!        hgs.nchosen(:)=.false.   ! clear list of chosen nodes
!
!        call OpenAscii(FnumRecharge,'recharge.ginc')
!        call Msg( 'HGS recharge iclude file: '//'recharge.ginc')
!        write(Fnumrecharge,'(a)') '! Created by program TG '//TGVersion//', Cross-section recharge include file dated '//DateSTR
!        write(Fnumrecharge,'(a)') '! ***THIS FILE MAY BE OVERWRITTEN so copy and edit'
!
!     
!        ! loop over table entries, i.e. recharge zones
!        do j=1,nTbl-1
!            xStart=xTbl(j)
!            xEnd=xTbl(j+1)
!            
!            ! loop over nodes in list and find start
!            do i=1,nx-1
!                x1=xi_temp(indx_col(i))
!                x2=xi_temp(indx_col(i+1))
!                if(x1 <= xStart .and. x2 >= xStart) then  ! found start
!                    frac=(xStart-x1)/(x2-x1)
!                    if(frac <= 0.5) then
!                        iStart=i
!                    else
!                        iStart=i+1
!                    end if
!                end if
!                  
!            end do
!            ! loop over nodes in list and find end
!            do i=iStart,nx-1
!                x1=xi_temp(indx_col(i))
!                x2=xi_temp(indx_col(i+1))
!                if(x1 <= xEnd .and. x2 >= xEnd) then  ! found end
!                    frac=(xEnd-x1)/(x2-x1)
!                    if(frac <= 0.5) then
!                        iEnd=i
!                    else
!                        iEnd=i+1
!                    end if
!                end if
!                  
!            end do
!            
!            if(j == nTbl-1) iEnd=nx
!            
!            TMPStr=FileNumberString(j)
!            call OpenAscii(FnumNodeSet,'recharge.node_list.'//trim(TMPStr))
!            call Msg( 'HGS node set file: '//'recharge.node_list.'//trim(TMPStr))
!            do i=istart,iend
!                iNode=hgs.NodeList(hgs.nNodeList,indx_col(i))
!			    write(FnumNodeSet,*) iNode
!			    write(FnumNodeSet,*) iNode-hgs.nn2d
!            end do
!            call freeunit(FnumNodeSet)
!            
!            write(Fnumrecharge,'(a)') '!------------------------------ Recharge zone '//trim(TMPStr)
!            write(Fnumrecharge,'(a,1pg12.5,a,1pg12.5)') '! From ',xStart,' to ',xEnd 
!            write(Fnumrecharge,'(a,1pg12.5)') '! Recharge conversion factor ',ConversionFactor
!            write(Fnumrecharge,'(a,1pg12.5)') '! Recharge input ',RechargeTbl(j)
!            write(Fnumrecharge,'(a,1pg12.5)') '! Recharge converted ',RechargeTbl(j)*ConversionFactor
!            write(Fnumrecharge,'(a)') 'clear chosen nodes'
!            write(Fnumrecharge,'(a)') 'choose nodes list'
!            write(Fnumrecharge,'(a)') '..\_recharge\recharge.node_list.'//trim(TMPStr)
!
!            write(Fnumrecharge,'(a)') '  create face set'
!            write(Fnumrecharge,'(a)') '  Rzone'//trim(TMPStr)
!
!            write(Fnumrecharge,'(a)') '  boundary condition'
!            write(Fnumrecharge,'(a)') '      type'
!            write(Fnumrecharge,'(a)') '      rain'
!
!            write(Fnumrecharge,'(a)') '      name'
!            write(Fnumrecharge,'(a)') '      Rzone'//trim(TMPStr)
!
!            write(Fnumrecharge,'(a)') '      face set'
!            write(Fnumrecharge,'(a)') '      Rzone'//trim(TMPStr)
!
!            write(Fnumrecharge,'(a)') '      time value table'
!            write(Fnumrecharge,'(a,1pg12.5)') '        0.0   ',RechargeTbl(j)*ConversionFactor
!            write(Fnumrecharge,'(a)') '      end'
!
!            write(Fnumrecharge,'(a)') '      tecplot output'
!            write(Fnumrecharge,'(a)') '  end'
!
!            
!        
!
!        end do
!
!        continue
!      
!
!  !      rewind(FNum)
!  !      do j=1,hgs.mesh.ne
!		!	write(FNum,'(i10,3e20.12)') j,kxx(j),kyy(j),kzz(j)
!		!end do
!  !
!  !      call FreeUnit(FNum)
!              
!
!   end subroutine HGS_RechargeCrossSection
!   
!    !------------------------------------------------------------------------
!    subroutine HGS_ReplaceDTW_With_FeflowSliceNumber(FnumTG,hgs)
!        implicit none
!
!        integer :: i,j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumFS
!        character(MAXLBL) :: FNameFS
!
!        type (HGSProject) hgs
!        
!        real(dr), allocatable :: FeflowSlce(:)
!        integer :: i1
!        character(MAXSTRING) :: line
!        
!        
!        allocate(FeflowSlce(hgs.mesh.nn))
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'pm.dat file: '//FName)
!        
!        i1=index(FName,'.dat')
!        FNameFS=FName(:i1)//'FSlice.dat'
!        call OpenAscii(FNumFS,FNameFS)
!        call Msg( 'pm.FSlice.dat file: '//FNameFS)
!
!        outer: do
!            read(FNum,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            if(index(line,'"Depth2GWT"')>0) then   ! replace variable name
!                i1=index(line,'"Depth2GWT"')
!                line=line(:i1)//'FeflowSlice"'//line(i1+11:)
!                write(FNumFS,'(a)') line
!            elseif(index(line,'# Depth2GWT')>0) then  ! replace Depth2GWT data with FeflowSlce data       
!                i1=index(line,'# Depth2GWT')
!                line=line(:i1)//' FeflowSlice'//line(i1+11:)
!                write(FNumFS,'(a)') line
!                ! replace with Feflow slice number
!                i1=0
!                do i=hgs.nsheet,1,-1
!                    do j=1,hgs.nn2d
!                        i1=i1+1
!                        FeflowSlce(i1)=i
!                    end do
!                end do
!                write(FNumFS,'(5g26.17e3)') (FeflowSlce(j),j=1,hgs.mesh.nn)
!                ! skip to next #
!                do 
!                    read(FNum,'(a)') line
!                    if(index(line,'#')>0) then
!                        backspace(fnum)
!                        exit
!                    end if
!                end do
!            else
!                write(FNumFS,'(a)') line
!                
!            end if
!        end do outer
!          
!        
!    end subroutine HGS_ReplaceDTW_With_FeflowSliceNumber
!    subroutine HGS_AddFeflowSliceNumber(FnumTG,hgs)
!        implicit none
!
!        integer :: i,j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumFS
!        character(MAXLBL) :: FNameFS
!
!        type (HGSProject) hgs
!        
!        real(dr), allocatable :: FeflowSlce(:)
!        integer :: i1
!        character(MAXSTRING) :: line
!        
!        integer :: NVar
!        
!        
!        allocate(FeflowSlce(hgs.mesh.nn))
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'pm.dat file: '//FName)
!        
!        i1=index(FName,'.dat')
!        FNameFS=FName(:i1)//'FSlice.dat'
!        call OpenAscii(FNumFS,FNameFS)
!        call Msg( 'pm.FSlice.dat file: '//FNameFS)
!
!        outer: do
!            read(FNum,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            if(index(line,'VARIABLES =')>0) then   ! append slice variable name
!                NVar=CountChars(COMMA,line)+2
!                line=line(:len_trim(line))//',"Slice"'
!                write(FNumFS,'(a)') trim(line)
!    
!                read(FNum,'(a)') line   ! read first Zone line
!                !l1= index(line,']=CELLCENTERED)')
!                !write(TMPStr,'(i5)') NVar
!                !line=line(:l1-1)//','//trim(TMPStr)//line(l1:)
!                write(FNumFS,'(a)') trim(line)
!               
!                
!            elseif(index(line,'# element node lists')>0) then  ! insert slice data       
!                write(FNumFS,'(a)') '# Feflow slice numbers inserted by TG'
!                ! create Feflow slice numbers
!                i1=0
!                do i=hgs.nsheet,1,-1
!                    do j=1,hgs.nn2d
!                        i1=i1+1
!                        FeflowSlce(i1)=i
!                    end do
!                end do
!                write(FNumFS,'(5g26.17e3)') (FeflowSlce(j),j=1,hgs.mesh.nn)
!                
!                ! write line
!                write(FNumFS,'(a)') trim(line)
!                ! write to next zone
!                do 
!                    read(FNum,'(a)',iostat=status) line
!                    if(status/=0) exit outer
!
!                    if(index(line,'ZONE  T=')>0) then
!                        backspace(fnum)
!                        exit
!                    else
!                        write(FNumFS,'(a)') trim(line)
!                    end if
!                end do
!            elseif(index(line,'ZONE  T=')>0) then  ! subsequent zones, update connectivity list       
!                !l1= index(line,']=CELLCENTERED)')
!                !write(TMPStr,'(i5)') NVar
!                !line=line(:l1-1)//','//trim(TMPStr)//line(l1:)
!
!                l1= index(line,']), CONNECTIVITYSHAREZONE=1')
!                write(TMPStr,'(i5)') NVar
!                line=line(:l1-1)//','//trim(TMPStr)//line(l1:)
!
!                write(FNumFS,'(a)') trim(line)
!
!            
!            else
!                write(FNumFS,'(a)') trim(line)
!                
!            end if
!        end do outer
!          
!        
!    end subroutine HGS_AddFeflowSliceNumber
!
!    !------------------------------------------------------------------------
!    subroutine HGS_CSVTableToMprops(FnumTG)
!        implicit none
!       
!        integer :: i
!        integer :: i1
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!
!        character(MAXLBL) :: line
!        
!        
!        integer :: nvar
!        integer :: nGWFMaterials
!        character(MAXLBL), allocatable :: ColumnName(:)
!        character(MAXLBL), allocatable ::   Description(:)
!        real(dr), allocatable ::            Kxx(:)
!        real(dr), allocatable ::            Kyy(:)
!        real(dr), allocatable ::            Kzz(:)
!        real(dr), allocatable ::            Porosity(:)
!        real(dr), allocatable ::            SpecificStorage(:)
!        character(MAXLBL), allocatable ::   SoilRetnFileName(:)
!        
!        character(MAXSTRING) :: PathPrefix
!        
!        real(dr) :: KMult
!
!       
!        
!        !  csv table file 
!        read(FnumTG,'(a)') FNameCSV
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg( 'Climate CSV file: '//FNameCSV)
!        
!        !  Path prefix  e.g. ..\ 
!        read(FnumTG,'(a)') PathPrefix
!        call Msg('Path prefix for includes: "'//trim(PathPrefix)//'"')
!        
!        ! Hydraulic conductivity multiplication factor
!        read(FnumTG,*) KMult
!        write(TMPStr,'(f12.5)') KMult
!        call Msg('Input hydraulic conductivity values in units of m/s will be multiplied by: '//trim(TMPStr))
!
!        
!        ! line 1  Header
!        read(FnumCSV,'(a)') line
!        nvar=CountChars(COMMA,line)
!        
!        allocate(ColumnName(nvar))
!        
!        i1=index(line,',')
!        line=line(i1+1:)
!        do i=1,nvar
!            i1=index(line,',')
!            ColumnName(i)=line(:i1-1)
!            line=line(i1+1:)
!        end do
!
!        nGWFMaterials=0
!        do ! count materials
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!
!            nGWFMaterials=nGWFMaterials+1
!        end do
!        
!        allocate(Description(nGWFMaterials), &
!                 Kxx(nGWFMaterials), &
!                 Kyy(nGWFMaterials), &
!                 Kzz(nGWFMaterials), &
!                 Porosity(nGWFMaterials), &
!                 SpecificStorage(nGWFMaterials), &
!                 SoilRetnFileName(nGWFMaterials))
!        
!        
!        
!        rewind(FnumCSV)
!        read(FnumCSV,'(a)') line
!        do i=1,nGWFMaterials
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,',')
!            read(line(:i1-1),'(a)') Description(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Kxx(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Kyy(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Kzz(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Porosity(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) SpecificStorage(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            if(i1>0) then
!                read(line(:i1-1),'(a)') SoilRetnFileName(i)
!            else
!                read(line,'(a)') SoilRetnFileName(i)
!            end if    
!        end do
!        
!        FName=trim(FNameCSV)//'.mprops'
!        call OpenAscii(FNum,FName)
!        call Msg( 'Mprops file: '//FName)
!        
!        write(FNum,'(a)') '! Mprops file written by TG from csv file '//trim(FNameCSV)
!        write(FNum,'(a,f12.5)') '! **** NOTE: Input K in m/s are multiplied by ',KMult
!
!
!        do i=1,nGWFMaterials
!            write(FNum,'(a)') '! -------------------------------------------------'
!            write(FNum,'(a,i5)') '! ------------------------------Material '
!            write(FNum,'(a,i5)') Description(i)
!            if(kxx(i)==Kyy(i) .and. kxx(i)==kzz(i)) then  ! isotropic
!                write(FNum,'(a)') 'k isotropic'
!                write(FNum,*) kxx(i)*KMult
!                write(FNum,*) '! input K(m/s): ',kxx(i)
!            else
!                write(FNum,'(a)') 'k anisotropic'
!                write(FNum,*) kxx(i)*KMult, kyy(i)*KMult, kzz(i)*KMult
!                write(FNum,*) '! input K(m/s): '
!                write(FNum,*) '! kxx: ',kxx(i)
!                write(FNum,*) '! kyy: ',kyy(i)
!                write(FNum,*) '! kzz: ',kzz(i)
!            end if
!            
!            write(FNum,'(a)') 'porosity'
!            write(FNum,*) porosity(i)
!            
!            write(FNum,'(a)') 'specific storage'
!            write(FNum,*) SpecificStorage(i)
!            
!            write(FNum,'(a)') '! Soil retention properties are read from an include file'
!            write(FNum,'(a)') 'include '//trim(PathPrefix)//trim(SoilRetnFileName(i))
!            
!            write(FNum,'(a)') 'end'
!            
!        end do
!        close(FNum)
!        
!        continue
!       
!    end subroutine HGS_CSVTableToMprops
!    !------------------------------------------------------------------------
!    subroutine HGS_CSVTableToObsPts(FnumTG)
!        implicit none
!       
!        integer :: i
!        integer :: i1
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!
!        integer :: FNumXYZ
!        character(MAXLBL) :: FNAMEXYZ 
!        
!        integer :: FNumLables
!        character(MAXLBL) :: FNAMELables 
!
!        character(MAXLBL) :: line
!        
!        
!        integer :: nvar
!        integer :: nObs
!        character(MAXLBL), allocatable :: ColumnName(:)
!        character(MAXLBL), allocatable ::   Description(:)
!        real(dr), allocatable ::            Easting(:)
!        real(dr), allocatable ::            Northing(:)
!        real(dr), allocatable ::            ScreenTop(:)
!        real(dr), allocatable ::            ScreenBot(:)
!        integer, allocatable  ::            IncludeFlag(:)
!
!        character(MAXLBL) ::   Dummy ! backwards compatibility for reading path
!     
!        !  csv table file 
!        read(FnumTG,'(a)') FNameCSV
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg( 'Climate CSV file: '//FNameCSV)
!
!        !  read dummy (path) 
!        read(FnumTG,'(a)')Dummy
!
!        
!        ! line 1  Header
!        read(FnumCSV,'(a)') line
!        nvar=CountChars(COMMA,line)
!        
!        allocate(ColumnName(nvar))
!        
!        i1=index(line,',')
!        line=line(i1+1:)
!        do i=1,nvar
!            i1=index(line,',')
!            ColumnName(i)=line(:i1-1)
!            line=line(i1+1:)
!        end do
!
!        nObs=0
!        do ! count materials
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!
!            nObs=nObs+1
!        end do
!        
!        allocate(Description(nObs), &
!            Easting(nObs), &
!            Northing(nObs), &
!            ScreenTop(nObs), &
!            ScreenBot(nObs), &
!            IncludeFlag(nObs))
!        
!        
!        
!        rewind(FnumCSV)
!        read(FnumCSV,'(a)') line
!        do i=1,nObs
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,',')
!            read(line(:i1-1),'(a)') Description(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Easting(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) Northing(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) ScreenTop(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) ScreenBot(i)
!            line=line(i1+1:)
!            
!            read(line,*) IncludeFlag(i)
!           
!        end do
!        
!       ! remove .csv from input file name
!        i1=index(FNameCSV,'.csv')
!        FNameCSV=FNameCSV(:i1-1)
!        
!        FName=trim(FNameCSV)//'.ginc'
!        call OpenAscii(FNum,FName)
!        call Msg( 'Observation point ginc file: '//FName)
!        
!        write(FNum,'(a)') '! *** EDITS TO THIS FILE MAY BE OVERWRITTEN BY TG *** '
!        write(FNum,'(a)') '! Observation point ginc file written by TG from csv file '//trim(FNameCSV)
!
!        do i=1,nObs
!            if(.not. IncludeFlag(i)) cycle
!            
!            if(ScreenTop(i)==ScreenBot(i)) then  ! point
!                write(FNum,'(a,i5)') '! ------------------------------Observation point'
!                write(FNum,'(a)') 'make observation point'
!                write(FNum,'(a)') trim(Description(i))
!                write(FNum,'(3f20.8)') Easting(i),Northing(i),ScreenTop(i)
!                write(FNum,'(a)') ' '
!
!            else
!                write(FNum,'(a,i5)') '! ------------------------------Observation well'
!                write(FNum,'(a)') 'make observation well'
!                write(FNum,'(a)') trim(Description(i))
!                write(FNum,'(3f20.8)') Easting(i),Northing(i),ScreenTop(i)
!                write(FNum,'(3f20.8)') Easting(i),Northing(i),ScreenBot(i)
!                write(FNum,'(a)') ' '
!
!            end if
!        end do
!        close(FNum)
!        
!        FNAMEXYZ=trim(FNameCSV)//'.xyz.dat'
!        call OpenAscii(FNumXYZ,FNAMEXYZ)
!        call Msg( 'Obs XYZ to file: '//FNAMEXYZ)
!        write(FNumXYZ,'(a)') 'Title = "'//trim(FNameCSV)//' XYZ for Tecplot"'
!        write(FNumXYZ,'(a)') 'VARIABLES = "X","Y","Z"'
!
!            
!        
!        write(FNumXYZ,'(a)') 'ZONE T="'//trim(FNameCSV)//'", STRANDID=0, DT=(Double, Double)'
!        do i=1,nObs
!            if(.not. IncludeFlag(i)) cycle
!            write(FNumXYZ,'(3f20.8)') Easting(i),Northing(i),ScreenTop(i)
!        end do
!
!        FNAMELables=trim(FNameCSV)//'.Lables.dat'
!        call OpenAscii(FNumLables,FNAMELables)
!        call Msg( 'Obs scatter/lables to file: '//FNAMELables)
!        write(FNumLables,'(a)') 'Title = "'//trim(FNameCSV)//' Labels for Tecplot"'
!
!        do i=1,nObs
!            if(.not. IncludeFlag(i)) cycle
!            write(FNumLables,'(a)') 'TEXT'
!            write(FNumLables,'(a)') 'CS=GRID3d'
!            write(FNumLables,'(2(a,f20.8))') 'X=',Easting(i),',Y=',Northing(i),',Z=',ScreenTop(i)
!            write(FNumLables,'(a)') 'C=BLACK '
!            write(FNumLables,'(a)') 'S=LOCAL'
!            write(FNumLables,'(a)') 'HU=POINT'
!            write(FNumLables,'(a)') 'LS=1 AN=Left'
!            write(FNumLables,'(a)') 'BX=NOBOX BXM=20 LT=0.1 BXO=BLACK BXF=WHITE '
!            write(FNumLables,'(a)') 'F=HELV'
!            write(FNumLables,'(a,I5,a)') 'H=10 A=0'
!            write(FNumLables,'(a)') 'MFC=""'
!            write(FNumLables,'(a)') 'CLIPPING=CLIPTOVIEWPORT'
!            write(TMPStr,'(a)') Description(i)(:len_trim(Description(i)))
!            write(FNumLables,'(a)') 'T="   '//trim(TMPStr)//'"'
!            write(FNumLables,'(a)') '#--------------------------------------------'
!        end do
!
!
!        
!        continue
!       
!    end subroutine HGS_CSVTableToObsPts
!    !------------------------------------------------------------------------
!    subroutine HGS_CalibrationSnapshot(FnumTG, hgs)
!        implicit none
!       
!        type (HGSProject) hgs  
!
!        integer :: i
!        integer :: i1
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!
!       
!        integer :: FNumLables
!        character(MAXLBL) :: FNAMELables 
!
!        character(MAXLBL) :: line
!        
!        
!        integer :: nvar
!        integer :: nObs
!        character(MAXLBL), allocatable :: ColumnName(:)
!        character(MAXLBL), allocatable ::   nameObs(:)
!        real(dr), allocatable ::            xObs(:)
!        real(dr), allocatable ::            yObs(:)
!        real(dr), allocatable ::            zObs(:)
!        real(dr), allocatable ::            hObs(:)
!        integer, allocatable  ::            groupObs(:)
!        
!        
!        !-----------------------------------------------------------------------
!        integer :: j
!        integer :: nClass=0
!        character(40), allocatable :: ClassObs(:) 
!        character(40), allocatable :: ClassName(:) 
!        integer, allocatable :: ClassFlag(:) 
!        integer, allocatable :: ClassNobs(:) 
!        logical :: ClassFound
!        real, allocatable :: hSim(:)
!        
!        Real(dr) :: YMax
!        Real(dr) :: YMin
!        Real(dr) :: YSpread
!        Real(dr) :: o_s
!        Real(dr) :: o_s2
!        Real(dr) :: Sum_o_s
!        Real(dr) :: Sum_o_s2
!        Real(dr) :: RMSD
!        Real(dr) :: NRMSD
!        
!        integer :: FNumScatter
!        character(MAXLBL) :: FNAMEScatter 
!       
!        integer :: TextHeight
!        !-------------------------------------------------------------
!        
!        integer :: nde
!        
!        !  csv table file 
!        read(FnumTG,'(a)') FNameCSV
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg( 'Climate CSV file: '//FNameCSV)
!
!        ! line 1  Header
!        read(FnumCSV,'(a)') line
!        nvar=CountChars(COMMA,line)
!        
!        allocate(ColumnName(nvar))
!        
!        i1=index(line,',')
!        line=line(i1+1:)
!        do i=1,nvar
!            i1=index(line,',')
!            ColumnName(i)=line(:i1-1)
!            line=line(i1+1:)
!        end do
!
!        nObs=0
!        do ! count calibration points
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!
!            nObs=nObs+1
!        end do
!        
!        allocate(NameObs(nObs), &
!            xObs(nObs), &
!            yObs(nObs), &
!            zObs(nObs), &
!            hObs(nObs), &
!            GroupObs(nObs))
!        
!        allocate(hSim(nObs),ClassObs(nObs), ClassName(nObs),ClassFlag(nObs),ClassnObs(nObs))
!        ClassFlag(:)=0
!        ClassnObs(:)=0
!        
!        
!        
!        rewind(FnumCSV)
!        read(FnumCSV,'(a)') line
!        do i=1,nObs
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,',')
!            read(line(:i1-1),'(a)') nameObs(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) xObs(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) yObs(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) zObs(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),*) hObs(i)
!            line=line(i1+1:)
!            
!            read(line,'(a)') classObs(i)
!           
!        end do
!        
!       ! remove .csv from input file name
!        i1=index(FNameCSV,'.csv')
!        FNameCSV=FNameCSV(:i1-1)
!        
!        
!        ! Build classes of observation data e.g. formation names
!        do i=1,nObs
!            if(nClass==0) then
!                nClass=nClass+1
!                ClassName(nClass)=ClassObs(i)
!                ClassFlag(i)=nClass
!            else
!                ClassFound=.false.
!                do j=1,nClass
!                    if(ClassObs(i)==ClassName(j)) then  ! flag as part of this class
!                        ClassFlag(i)=j
!                        ClassnObs(j)=ClassnObs(j)+1
!                        ClassFound=.true.
!                        exit
!                    end if
!                end do
!                if(.not. ClassFound) then
!                    nClass=nClass+1
!                    ClassName(nClass)=ClassObs(i)
!                    ClassFlag(i)=nClass
!                    ClassnObs(nClass)=ClassnObs(nClass)+1
!                end if
!            end if
!                   
!            if(i>1) then
!                do j=1,i-1
!                    if(nameObs(j)==nameObs(i)) then
!                        call Msg('Duplicate observation point names: '//nameObs(i))
!                        stop
!                    end if
!                end do
!            end if
!        end do
!        
!        
!        ! Find simulated head data from XYZ obs data
!        do i=1,nObs
!            call find_node(xObs(i),yObs(i),zObs(i),nde,hgs)
!            hSim(i)=hgs.Head(nde)
!        end do
!
!        !  45 degree reference line        
!        FNAME='45DegreeReferenceLine.dat'
!        call OpenAscii(FNum,FName)
!        write(FNum,'(a)') 'variables="Observed","Simulated"'
!        write(FNum,'(a)') 'zone t="45degree line"'
!        YMax=maxval(hobs)
!        YMin=minval(hobs)
!        write(FNum,*) YMin-0.1*(Ymax-YMin), YMin-0.1*(Ymax-YMin)
!        write(FNum,*) YMax+0.1*(Ymax-YMin), YMax+0.1*(Ymax-YMin)
!        close(FNum)
!        
!        
!        !  All observation points: 45 degree plots, data, stats and Labels in separate files        
!        FNAME=trim(FNameCSV)//'_45DegreeData.dat'
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//FName)
!        
!        write(FNum,'(a)') 'variables="Observed","Simulated"'
!        
!        !write(FNum,'(a)') 'zone t="45degree line"'
!        YMax=maxval(hobs)
!        YMin=minval(hobs)
!        !
!        !write(FNum,*) YMin, YMin
!        !write(FNum,*) YMax, YMax
!       
!        write(FNum,'(a)') 'zone t="All"'
!        
!        Sum_o_s=0.0d0
!        Sum_o_s2=0.0d0
!        do i=1,nObs
!            write(FNum,*) hObs(i), hSim(i)
!            o_s= hObs(i)-hSim(i)
!            o_s2=o_s*o_s
!            Sum_o_s=Sum_o_s+o_s
!            Sum_o_s2=Sum_o_s2+o_s2
!        end do
!
!        call freeunit(Fnum)
!
!        RMSD=sqrt(Sum_o_s2/nObs)
!        YSpread=Ymax-Ymin
!        if(YSpread==0) then
!            NRMSD=-999.
!        else
!            NRMSD=RMSD/YSpread*100.00d0
!        end if
!
!        Write(*,*) 'N ',nObs
!        Write(*,*) 'YMax ',YMax
!        Write(*,*) 'YMin ',YMin
!        Write(*,*) 'Ymax-Ymin ',YSpread
!        Write(*,*) 'RMSD ',RMSD
!        Write(*,*) 'NRMSD ',NRMSD
!
!        read(FNumTG,*) Textheight
!        
!        
!        FNAME=trim(FNameCSV)//'_45DegreeStatistics.dat'
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//FName)
! 
!        write(FNum,'(a)') 'variables="N","YMax","YMin","YMax-YMin","RMSD","NRMSD"'
!        
!        write(FNum,'(a)') 'zone t="Calibration Statistics"'
!        write(FNum,'(i5,5f15.3)') nObs, YMax, YMin, YSpread, RMSD, NRMSD
!
!        call freeunit(Fnum)
!        FNAME=trim(FNameCSV)//'_45DegreeLabels.dat'
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//FName)
!        
!        do i=1,nObs
!            write(FNum,'(a)') 'TEXT'
!            write(FNum,'(a)') 'CS=GRID'
!            write(FNum,'(2(a,f20.8))') 'X=',hObs(i),',Y=',hSim(i)
!            write(FNum,'(a)') 'C=BLACK '
!            write(FNum,'(a)') 'S=LOCAL'
!            write(FNum,'(a)') 'HU=POINT'
!            write(FNum,'(a)') 'LS=1 AN=MIDLEFT'
!            write(FNum,'(a)') 'F=HELV'
!            write(FNum,'(a,I5,a)') 'H=',Textheight,' A=0'
!            write(FNum,'(a)') 'MFC=""'
!            write(FNum,'(a)') 'CLIPPING=CLIPTOVIEWPORT'
!            write(FNum,'(a)') 'T="   '//nameObs(i)(:len_trim(nameObs(i)))//'"'
!            write(FNum,'(a)') '#--------------------------------------------'
!        end do
!        call freeunit(Fnum)
!
!
!        ! Classified observation points: 3D scatter plot, 45 degree plots, data, stats and Labels in separate files
!        ! 3D scatter plot with classes
!        FNAMEScatter=trim(FNameCSV)//'_3DScatterData.dat'
!	    call getunit(FNumScatter)
!	    OPEN(FNumScatter,file =FNAMEScatter,status = 'replace',form = 'formatted',iostat = status)
!	    write(FNumScatter,'(a)') 'Title = " Observed vs simulated heads scatter plot"'
!	    write(FNumScatter,'(a)') 'VARIABLES = "X","Y","Z","Observed","Simulated","Error"'
!        
!        ! Classed stats
!        do j=1,nclass
!            write(FNumScatter,'(a)') 'ZONE T="'//trim(ClassName(j))//'"'
!            FNAME=trim(FNameCSV)//'_'//trim(ClassName(j))//'_45DegreeData.dat'
!            call OpenAscii(FNum,FName)
!            call Msg( 'To File: '//FName)
!            
!            FNAMELables=trim(FNameCSV)//'_'//trim(ClassName(j))//'_45DegreeLabels.dat'
!            call OpenAscii(FNumLables,FNAMELables)
!            call Msg( 'Labels to File: '//FNAMELables)
!        
!            write(FNum,'(a)') 'variables="Observed","Simulated"'
!        
!            !write(FNum,'(a)') 'zone t="'//trim(ClassName(j))//' 45degree line"'
!            YMax=-1e20
!            YMin=1e20
!            do i=1,nObs
!                if(ClassFlag(i)==j) then
!                    if( hObs(i)>YMax) YMax=hObs(i)
!                    if( hObs(i)<YMin) YMin=hObs(i)
!                end if
!            end do
!            !write(FNum,*) YMin, YMin
!            !write(FNum,*) YMax, YMax
!            
!            write(FNum,'(a)') 'zone t="'//trim(ClassName(j))//'"'
!            Sum_o_s=0.0d0
!            Sum_o_s2=0.0d0
!            do i=1,nObs
!                if(ClassFlag(i)==j) then
!                    write(FNum,*) hObs(i),  hSim(i)
!                    write(FNumScatter,'(6e20.8)') xobs(i),yobs(i),zobs(i),hobs(i),hSim(i),hSim(i)-hobs(i)
!                    o_s= hObs(i)-hSim(i)
!                    o_s2=o_s*o_s
!                    Sum_o_s=Sum_o_s+o_s
!                    Sum_o_s2=Sum_o_s2+o_s2
!                    
!                    write(FNumLables,'(a)') 'TEXT'
!                    write(FNumLables,'(a)') 'CS=GRID'
!                    write(FNumLables,'(2(a,f20.8))') 'X=',hObs(i),',Y=',hSim(i)
!                    write(FNumLables,'(a)') 'C=BLACK '
!                    write(FNumLables,'(a)') 'S=LOCAL'
!                    write(FNumLables,'(a)') 'HU=POINT'
!                    write(FNumLables,'(a)') 'LS=1 AN=MIDLEFT'
!                    write(FNumLables,'(a)') 'F=HELV'
!                    write(FNumLables,'(a,I5,a)') 'H=',Textheight,' A=0'
!                    write(FNumLables,'(a)') 'MFC=""'
!                    write(FNumLables,'(a)') 'CLIPPING=CLIPTOVIEWPORT'
!                    write(FNumLables,'(a)') 'T="   '//nameObs(i)(:len_trim(nameObs(i)))//'"'
!                    write(FNumLables,'(a)') '#--------------------------------------------'
!                end if
!            end do
!            YSpread=Ymax-Ymin
!            RMSD=sqrt(Sum_o_s2/ClassnObs(j))
!            if(YSpread==0) then
!                NRMSD=-999.
!            else
!                NRMSD=RMSD/YSpread*100.00d0
!            end if
!
!            call freeunit(Fnum)
!    
!            FNAME=trim(FNameCSV)//'_'//trim(ClassName(j))//'_45DegreeStatistics.dat'
!            call OpenAscii(FNum,FName)
!            call Msg( 'To File: '//FName)
! 
!            write(FNum,'(a)') 'variables="N","YMax","YMin","YMax-YMin","RMSD","NRMSD"'
!        
!            write(FNum,'(a)') 'zone t="'//trim(ClassName(j))//' Calibration Statistics"'
!            write(FNum,'(i5,5f15.3)') ClassnObs(j), YMax, YMin, YSpread, RMSD, NRMSD
!            
!            call freeunit(Fnum)
!        end do
!
!        
!        continue
!
!        read(FNumTG,*) Textheight
!
!        FNAME=trim(FNameCSV)//'_3DScatterLabels.dat'
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//FName)
!
!        do i=1,nObs
!            write(FNum,'(a)') 'TEXT'
!            write(FNum,'(a)') 'CS=GRID3d'
!            write(FNum,'(2(a,f20.8))') 'X=',xObs(i),',Y=',yObs(i),',Z=',zObs(i)
!            write(FNum,'(a)') 'C=BLACK '
!            write(FNum,'(a)') 'S=LOCAL'
!            write(FNum,'(a)') 'HU=POINT'
!            write(FNum,'(a)') 'LS=1 AN=Left'
!            write(FNum,'(a)') 'F=HELV'
!            write(FNum,'(a,I5,a)') 'H=',Textheight,' A=0'
!            write(FNum,'(a)') 'MFC=""'
!            write(FNum,'(a)') 'CLIPPING=CLIPTOVIEWPORT'
!            write(TMPStr,'(3(a,f10.1))') '   '//nameObs(i)(:len_trim(nameObs(i)))//'\\n O:',hObs(i),'\\n S:',hSim(i),'\\n',hSim(i)-hObs(i)
!            write(FNum,'(a)') 'T="'//trim(TMPStr)//'"'
!            write(FNum,'(a)') '#--------------------------------------------'
!        end do
!        
!        continue
!       
!    end subroutine HGS_CalibrationSnapshot
!    !------------------------------------------------------------------------
!    subroutine HGS_CSVFileToTransCalibPlots(FnumTG)
!        implicit none
!       
!        integer :: i
!        integer :: i1, i2
!        
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!        character(MAXSTRING) :: PathSim
!        character(MAXSTRING) :: PathField
!
!        
!
!        character(MAXLBL) :: line
!        
!        
!        integer :: nvar
!        integer :: nObs
!        character(MAXLBL), allocatable :: ColumnName(:)
!        character(MAXLBL), allocatable ::   SimFile(:)
!        character(MAXLBL), allocatable ::   FieldFile(:)
!        integer, allocatable  ::            IncludeFlag(:)
!        
!       
!        logical :: TecplotLayoutFileExists
!        integer :: FNumTecMacro
!
!        logical :: ViewFit, XFit, YFit
!        real(dr) :: XPadRange, YPadRange
!
!        character(MAXSTRING) :: ObsName
!
!        character(MAXSTRING) :: FnamePS
!        character(MAXSTRING) :: FnamePDF(1000)
!        
!        character(5000) :: TMPStr2
!        character(MAXSTRING) :: CMDString
!
!        
!        !  csv table file 
!        read(FnumTG,'(a)') FNameCSV
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg( 'Climate CSV file: '//FNameCSV)
!        
!        read(FnumTG,'(a)') PathSim
!        call Msg( 'Path to simulation output files: '//PathSim)
!
!        read(FnumTG,'(a)') PathField
!        call Msg( 'Path field observation data files: '//PathField)
!        
!        ! line 1  Header
!        read(FnumCSV,'(a)') line
!        nvar=CountChars(COMMA,line)
!        
!        allocate(ColumnName(nvar))
!        
!        i1=index(line,',')
!        line=line(i1+1:)
!        do i=1,nvar
!            i1=index(line,',')
!            ColumnName(i)=line(:i1-1)
!            line=line(i1+1:)
!        end do
!
!        nObs=0
!        do ! count filwes
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!
!            nObs=nObs+1
!        end do
!        
!        allocate(SimFile(nObs), &
!            FieldFile(nObs), &
!            IncludeFlag(nObs))
!        
!        
!        
!        rewind(FnumCSV)
!        read(FnumCSV,'(a)') line
!        do i=1,nObs
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,',')
!            read(line(:i1-1),'(a)') SimFile(i)
!            line=line(i1+1:)
!            
!            i1=index(line,',')
!            read(line(:i1-1),'(a)') FieldFile(i)
!            line=line(i1+1:)
!            
!            read(line,*) IncludeFlag(i)
!           
!        end do
!        
!       ! remove .csv from input file name
!        i1=index(FNameCSV,'.csv')
!        FNameCSV=FNameCSV(:i1-1)
!
!        ! if Tecplot layout file with correct prefix (i.e. matches thisInfoLbl) exists
!        ! then set up the process for generating .ps and .pdf files, then concatentating .pdf's into one file
!        inquire(file=trim(FNameCSV)//'.lay',exist=TecplotLayoutFileExists)
!        if(TecplotLayoutFileExists) then  ! Start writing tecplot macro file
!            ! tecplot macro commands
!            FName=trim(FNameCSV)//'.mcr'
!            call OpenAscii(FNumTecMacro,FName)
!            write(FNumTecMacro,'(a)') '#!MC 1410'
!            write(FNumTecMacro,'(a)') '$!EXPORTSETUP EXPORTFORMAT = PS'
!            write(FNumTecMacro,'(a)') '$!PRINTSETUP PALETTE = COLOR'
!            FName='"'//trim(FNameCSV)//'.lay"'
!            write(FNumTecMacro,'(a)') '$!OPENLAYOUT '//FName
!            write(FNumTecMacro,'(a)') '$!EXPORTSETUP PRINTRENDERTYPE = VECTOR'
!        else 
!            call ErrMsg('No matching layout file: '//trim(FNameCSV)//'.lay') 
!        end if
!        !
!        ViewFit=.false.
!        read(FnumTG,*) ViewFit
!        
!        XFit=.false.
!        read(FnumTG,*) XFit
!        read(FnumTG,*) XPadrange
!        
!        YFit=.false.
!        read(FnumTG,*) YFit
!        read(FnumTG,*) YPadrange
!        
!        do i=1,nObs
!            
!            if(.not. IncludeFlag(i)) cycle
!            
!            write(FNumTecMacro,'(a)') '$!READDATASET  ''"'//trim(PathField)//trim(FieldFile(i))//'"'''
!            call msg(trim(PathField)//trim(FieldFile(i)))
!            write(FNumTecMacro,'(a)') 'ReadDataOption = New'
!            write(FNumTecMacro,'(a)') 'ResetStyle = No'
!            write(FNumTecMacro,'(a)') 'VarLoadMode = ByName'
!            write(FNumTecMacro,'(a)') 'AssignStrandIDs = Yes'
!
!            write(FNumTecMacro,'(a)') '$!READDATASET  ''"'//trim(PathSim)//trim(SimFile(i))//'"'''
!            call msg(trim(PathSim)//trim(SimFile(i)))
!            write(FNumTecMacro,'(a)') '  ReadDataOption = Append'
!            write(FNumTecMacro,'(a)') 'ResetStyle = No'
!            write(FNumTecMacro,'(a)') 'VarLoadMode = ByName'
!            write(FNumTecMacro,'(a)') 'AssignStrandIDs = Yes'
!            write(FNumTecMacro,'(a)') '$!CreateLineMap '
!            write(FNumTecMacro,'(a)') '$!DuplicateLineMap' 
!            write(FNumTecMacro,'(a)') 'SourceMap = 1'
!            write(FNumTecMacro,'(a)') 'DestinationMap = 2'
!            write(FNumTecMacro,'(a)') '$!LineMap [2]  Assign{Zone = 2}'
!            write(FNumTecMacro,'(a)') '$!ActiveLineMaps += [2]'
!            write(FNumTecMacro,'(a)') '$!LineMap [2]  Assign{XAxisVar = 3}'
!            write(FNumTecMacro,'(a)') '$!LineMap [2]  Assign{YAxisVar = 4}'
!            write(FNumTecMacro,'(a)') '$!LineMap [2]  Symbols{Show = No}'
!            write(FNumTecMacro,'(a)') '$!LineMap [2]  Lines{Show = Yes}'
!            write(FNumTecMacro,'(a)') '$!RenameDataSetVar' 
!            write(FNumTecMacro,'(a)') 'Var = 2'
!            write(FNumTecMacro,'(a)') 'Name = ''Observed'''
!            write(FNumTecMacro,'(a)') '$!RenameDataSetVar' 
!            write(FNumTecMacro,'(a)') 'Var = 4'
!            write(FNumTecMacro,'(a)') 'Name = ''Simulated'''
!            if(ViewFit) then
!                write(FNumTecMacro,'(a)') '$!View NiceFit'
!            end if
!            if(XFit) then
!                write(TmpSTR,'(1x,F10.2)') XPadrange
!                write(FNumTecMacro,'(a)') '$!VARSET |A1|= (|MINVAR[1]|-'//trim(TMPStr)//')'
!                write(FNumTecMacro,'(a)') '$!XYLINEAXIS XDETAIL 1 {RANGEMIN = |A1|}'
!                write(FNumTecMacro,'(a)') '$!VARSET |A2|= (|MAXVAR[1]|+'//trim(TMPStr)//')'
!                write(FNumTecMacro,'(a)') '$!XYLINEAXIS XDETAIL 1 {RANGEMAX = |A2|}'
!            end if
!            if(YFit) then
!                if(YPadrange<0.) then   !  Padrange treated as a fixed but sliding y-scale range 
!                    write(TmpSTR,'(1x,F10.2)') abs(YPadrange)
!                    write(FNumTecMacro,'(a)') '$!VARSET |A3|= (|MINVAR[2]|-('//trim(TMPStr)//'-(|MAXVAR[2]|-|MINVAR[2]|))/2.0)'
!                    write(FNumTecMacro,'(a)') '$!XYLINEAXIS YDETAIL 1 {RANGEMIN = |A3|}'
!                    write(FNumTecMacro,'(a)') '$!VARSET |A4|= (|MAXVAR[2]|+('//trim(TMPStr)//'-(|MAXVAR[2]|-|MINVAR[2]|))/2.0)'
!                    write(FNumTecMacro,'(a)') '$!XYLINEAXIS YDETAIL 1 {RANGEMAX = |A4|}'
!                else
!                    write(TmpSTR,'(1x,F10.2)') YPadrange
!                    write(FNumTecMacro,'(a)') '$!VARSET |A3|= (|MINVAR[2]|-'//trim(TMPStr)//')'
!                    write(FNumTecMacro,'(a)') '$!XYLINEAXIS YDETAIL 1 {RANGEMIN = |A3|}'
!                    write(FNumTecMacro,'(a)') '$!VARSET |A4|= (|MAXVAR[2]|+'//trim(TMPStr)//')'
!                    write(FNumTecMacro,'(a)') '$!XYLINEAXIS YDETAIL 1 {RANGEMAX = |A4|}'
!                end if
!            end if
!
!            i1=index(SimFile(i),'flow.')+5
!            i2=index(SimFile(i),'.dat')-1
!            obsname=Simfile(i)(i1:i2)
!                
!            FNamePS=trim(obsname)//'.ps'
!            FNamePDF(i)=trim(obsname)//'.pdf'
!
!            write(FNumTecMacro,'(a)') '$!EXPORTSETUP EXPORTFNAME = '//'"'//FNamePS(:len_trim(FNamePS))//'"'
!            write(FNumTecMacro,'(a)') '$!EXPORT'
!            write(FNumTecMacro,'(a)') 'EXPORTREGION = ALLFRAMES'
!        end do
!
!            
!        write(FNumTecMacro,'(a)') '$!QUIT'
!        call FreeUnit(FNumTecMacro)
!        
!        FName=trim(FNameCSV)//'.bat'
!        call OpenAscii(FNum,FName)
!        write(FNum,'(a)') 'echo'
!        write(FNum,'(a)') 'del *.ps'
!        write(FNum,'(a)') 'del *.pdf'
!        write(FNum,'(a)') 'tec360 -b -p '//trim(FNameCSV)//'.mcr'
!        write(FNum,'(a)') 'for %%f in (*.ps) do call ps2pdf -dEPSCrop "%%f"'
!        write(TMPStr2,'(a)') 'gswin64 -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -dAutoRotatePages=/None -sOutputFile='//trim(FNameCSV)//'.pdf'
!
!        do i=1,nObs
!            if(IncludeFlag(i)) TMPStr2=trim(TMPStr2)//' "'//trim(FNamePDF(i))//'"'
!        end do
!        write(FNum,'(a)') trim(TMPStr2)
!        call FreeUnit(FNum)
!        
!      	CMDString=trim(FNameCSV)//'.bat' 
!    	i=system(CMDString)
!        
!        
!        
!        continue
!       
!    end subroutine HGS_CSVFileToTransCalibPlots
!    !------------------------------------------------------------------------
!   subroutine HGS_BuildCustomZoneNames(FnumTG,hgs)
!        ! Get the materials used in the zones of the current HGS model and build the tecplot customlables file
!   
!        implicit none
!        
!        integer :: FnumTG
!        
!        integer :: FnumEco
!        character(MAXLBL) :: FNameEco
!
!        integer :: FNumCustomLabels
!        character(MAXLBL) :: FNameCustomLabels
!
!        type (HGSProject) hgs
!        
!        character(MAXLBL) :: line
!        integer :: i1
!        integer :: iZone
!        character(MAXLBL) :: LocalPrefix
!        
!        integer :: newIchar
!        
!
!        read(FNumTG,'(a)') LocalPrefix
!        FNameEco=trim(LocalPrefix)//'o.eco'
!        call OpenAscii(FnumEco,FNameEco)
!        call Msg( 'HGS eco file: '//trim(FNameEco))
!
!     
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            if(index(line,'POROUS MEDIA DOMAIN PROPERTIES')>0) exit
!        end do
!
!        FNameCustomLabels=trim(LocalPrefix)//'o.PM_CustomLabels.dat'
!        call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!        call Msg( 'HGS PM tecplot custom labels file: '//trim(FNameCustomLabels))
!        write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!
!        
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'DUAL CONTINUUM PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                read(FNumEco,'(a)') line
!                read(FNumEco,'(a)') line
!                read(line(2:),'(a)') hgs.PMZoneName(iZone)
!                
!                if(ichar(hgs.PMZoneName(iZone)(1:1))>=97 .and. ichar(hgs.PMZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.PMZoneName(iZone)(1:1))-(97-65)
!                    hgs.PMZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.PMZoneName(iZone))//'"'
!            end if
!        end do
!        
!        
!
!        izone=0
!        hgs.ZoneName(:)='empty'
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'FRACTURE DOMAIN PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                if(iZone==1) then
!                    FNameCustomLabels=trim(LocalPrefix)//'o.DUAL_CustomLabels.dat'
!                    call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!                    call Msg( 'HGS DUAL zone tecplot custom labels file: '//trim(FNameCustomLabels))
!                    write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!                end if
!                read(FNumEco,'(a)') line
!                read(FNumEco,'(a)') line
!                read(line(2:),'(a)') hgs.ZoneName(iZone)
!                
!                if(ichar(hgs.ZoneName(iZone)(1:1))>=97 .and. ichar(hgs.ZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.ZoneName(iZone)(1:1))-(97-65)
!                    hgs.ZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.ZoneName(iZone))//'"'
!            end if
!        end do
!
!
!        izone=0
!        hgs.ZoneName(:)='empty'
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'SURFACE DOMAIN PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                if(iZone==1) then
!                    FNameCustomLabels=trim(LocalPrefix)//'o.FRAC_CustomLabels.dat'
!                    call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!                    call Msg( 'HGS FRAC zone tecplot custom labels file: '//trim(FNameCustomLabels))
!                    write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!                end if
!                read(FNumEco,'(a)') line
!                read(FNumEco,'(a)') line
!                read(line(2:),'(a)') hgs.ZoneName(iZone)
!                
!                if(ichar(hgs.ZoneName(iZone)(1:1))>=97 .and. ichar(hgs.ZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.ZoneName(iZone)(1:1))-(97-65)
!                    hgs.ZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.ZoneName(iZone))//'"'
!            end if
!        end do
!        
!
!        izone=0
!        hgs.ZoneName(:)='empty'
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'CHANNEL FLOW PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                if(iZone==1) then
!                    FNameCustomLabels=trim(LocalPrefix)//'o.OLF_CustomLabels.dat'
!                    call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!                    call Msg( 'HGS OLF zone tecplot custom labels file: '//trim(FNameCustomLabels))
!                    write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!                end if
!                read(FNumEco,'(a)') line
!                read(line(12:),'(a)') hgs.ZoneName(iZone)
!                
!                if(ichar(hgs.ZoneName(iZone)(1:1))>=97 .and. ichar(hgs.ZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.ZoneName(iZone)(1:1))-(97-65)
!                    hgs.ZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.ZoneName(iZone))//'"'
!            end if
!        end do
!
!        izone=0
!        hgs.ZoneName(:)='empty'
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'WELL FLOW PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                if(iZone==1) then
!                    FNameCustomLabels=trim(LocalPrefix)//'o.CHAN_CustomLabels.dat'
!                    call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!                    call Msg( 'HGS CHAN zone tecplot custom labels file: '//trim(FNameCustomLabels))
!                    write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!                end if
!                
!                read(FNumEco,'(a)') line
!                read(FNumEco,'(a)') line
!                read(line(2:),'(a)') hgs.ZoneName(iZone)
!                
!                if(ichar(hgs.ZoneName(iZone)(1:1))>=97 .and. ichar(hgs.ZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.ZoneName(iZone)(1:1))-(97-65)
!                    hgs.ZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.ZoneName(iZone))//'"'
!            end if
!        end do
!
!
!        izone=0
!        hgs.ZoneName(:)='empty'
!        do
!            read(FNumEco,'(a)',iostat=status) line
!            if(status/=0) exit
!            
!            i1=index(line,'WELL FLOW PROPERTIES')
!            if (i1>0) exit
!            
!            i1=index(line,'ZONE:')
!            if(i1>0) then
!        
!                read(line(i1+6:),*) iZone
!                if(iZone==1) then
!                    FNameCustomLabels=trim(LocalPrefix)//'o.WELL_CustomLabels.dat'
!                    call OpenAscii(FNumCustomLabels,FNameCustomLabels)
!                    call Msg( 'HGS WELL zone tecplot custom labels file: '//trim(FNameCustomLabels))
!                    write(FNumCustomLabels,'(a)') 'CUSTOMLABELS'
!                end if
!                read(FNumEco,'(a)') line
!                read(line(12:),'(a)') hgs.ZoneName(iZone)
!                
!                if(ichar(hgs.ZoneName(iZone)(1:1))>=97 .and. ichar(hgs.ZoneName(iZone)(1:1))<=122) then  ! a-z in column 1, convert to upper case
!                    newIchar=ichar(hgs.ZoneName(iZone)(1:1))-(97-65)
!                    hgs.ZoneName(iZone)(1:1)=char(newIchar)
!                end if
!
!                
!                TmpSTR=FileNumberString(iZone)
!                write(FNumCustomLabels,'(a)') '"'//trim(TmpSTR)//' '//trim(hgs.ZoneName(iZone))//'"'
!            end if
!        end do
!
!        continue
!
!   end subroutine HGS_BuildCustomZoneNames
!
!    !------------------------------------------------------------------------
!   subroutine HGS_ElementalKZoneTable(FnumTG,hgs)
!        ! For 
!
!        implicit none
!
!        integer :: FnumTG
!
!        integer :: i, j
!
!        type (HGSProject) hgs
!
!        integer :: maxZone
!        logical :: ZoneUsed(1000)
!        integer :: ZoneECount(1000)
!
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        
!        real(dr),allocatable :: minKx(:)
!        real(dr),allocatable :: maxKx(:)
!        real(dr),allocatable :: minKy(:)
!        real(dr),allocatable :: maxKy(:)
!        real(dr),allocatable :: minKz(:)
!        real(dr),allocatable :: maxKz(:)
!
!        character(MAXLBL) :: frmt
!        character(MAXLBL) :: LocalPrefix
!
!        read(FNumTG,'(a)') LocalPrefix
!
!        
!        ZoneUsed(:)=.false.
!        ZoneECount(:)=0
!
!
!        do i=1,hgs.mesh.ne
!            ZoneUsed(hgs.iprop(i))=.true.
!            ZoneECount(hgs.iprop(i))=ZoneECount(hgs.iprop(i))+1
!        end do
!
!        maxZone=maxval(hgs.iprop)
!        allocate(minKx(maxZone), &
!                & maxKx(maxZone), &
!                & minKy(maxZone), &
!                & maxKy(maxZone), &
!                & minKz(maxZone), &
!                & maxKz(maxZone))
!
!        minKx(:)=1e20
!        maxKx(:)=-1e20
!        minKy(:)=1e20
!        maxKy(:)=-1e20
!        minKz(:)=1e20
!        maxKz(:)=-1e20
!        
!        If(.not. allocated(hgs.ElemKx)) then
!            call msg('No variable element K array allocated.  Possible reasons:')
!            call msg('  1) You did not read a file of variable element K')
!            call msg('      - These filenames contain the string "o.ElemK_pm"')
!            call msg('      - You can read it first using the instruction')
!            call msg('          "read elemental k file" ')
!            call msg('  2) You tried to read a file of variable element K but it failed')
!            call msg('      - Check: Is the filename spelled correctly?')
!            call msg('      - Yes, check: Does the file exist?')
!            call msg('          - No: This problem uses zoned K, not variable element K')
!            call msg('              - You can examine the zoned K values directly in the ')
!            call msg('                "Summary" section of the "o.eco" file')
!            return
!        end if
!
!        
!        do i=1,maxZone
!            do j=1,hgs.mesh.ne
!                if(hgs.iprop(j)==i) then ! update zone stats with info for this element
!                    if(hgs.ElemKx(j) < 0.0) then
!                        continue
!                    end if
!                    minKx(i)=min(minKx(i),hgs.ElemKx(j))
!                    maxKx(i)=max(maxKx(i),hgs.ElemKx(j))
!                    minKy(i)=min(minKy(i),hgs.ElemKy(j))
!                    maxKy(i)=max(maxKy(i),hgs.ElemKy(j))
!                    minKz(i)=min(minKz(i),hgs.ElemKz(j))
!                    maxKz(i)=max(maxKz(i),hgs.ElemKz(j))
!                end if
!            end do 
!        end do
!        
!        frmt='(i5,a,a,a,i8,6(a,1pg18.5))'
!        FNAME=trim(LocalPrefix)//'o.ElementalKZoneTable.csv'
!        call OpenAscii(FNum,FName)
!        call Msg( 'Zone analysis csv file: '//FName)
!        
!        write(FNum,'(a)') '"Zone","Material","#Elements","KxMin","KxMax","KyMin","KyMax","KzMin","KzxMax"'
!        do i=1,maxZone
!            write(FNum,frmt) i,',',trim(hgs.PMZonename(i)),',',ZoneECount(i),',',minKx(i),',',maxKx(i),',',minKy(i),',',maxKy(i),',',minKz(i),',',maxKz(i)            
!        end do
!        
!        call freeunit(Fnum)
!
!
!   end subroutine HGS_ElementalKZoneTable
!   
!    !!!subroutine HGS_HGSDirToTecplot(FnumTG,hgs)
!    !!!    
!    !!!    !Given an hgs dataset 
!    !!!    !write nodes xyz as IJK zone 
!    !!!    !write nodes and elements as FE zone  
!    !!!    
!    !!!    
!    !!!
!    !!!    INCLUDE 'tecio.f90'
!    !!!    type (HGSProject) hgs
!    !!!
!    !!!    ! rgm added
!    !!!    integer :: i, j
!    !!!    integer :: FnumTG
!    !!!    INTEGER*4, allocatable :: NData(:, :)
!    !!!    integer :: i1
!    !!!
!    !!!    
!    !!!    character*1 NULLCHR
!    !!!    integer :: Imax, Kmax, Jmax
!    !!!    Integer*4   Debug,isDouble
!    !!!
!    !!!   
!    !!!    NULLCHR = CHAR(0)
!    !!!    NullPtr = 0
!    !!!    Debug   = 1
!    !!!    FileType = 0
!    !!!    FileFormat = 0 ! 0 = PLT, 1 = SZPLT
!    !!!    VIsDouble = 0
!    !!!    IMax    = hgs.mesh.NN
!    !!!    JMax    = hgs.mesh.NE
!    !!!    KMax    = 1
!    !!!    ZoneType = 5 ! FEBRICK
!    !!!    SolTime = 360.0
!    !!!    StrandID = 0
!    !!!    ParentZn = 0
!    !!!    IsBlock = 1
!    !!!    ICellMax = 0
!    !!!    JCellMax = 0
!    !!!    KCellMax = 0
!    !!!    NFConns = 0
!    !!!    FNMode = 0
!    !!!    ShrConn = 0
!    !!!    !
!    !!!    !... Open the file and write the tecplot datafile
!    !!!    !... header information.
!    !!!    !
!    !!!    I = TecIni142('HGS Data set'//NULLCHR, &
!    !!!                'X Y Z'//NULLCHR, &
!    !!!                'HGStest1.plt'//NULLCHR, &
!    !!!                '.'//NULLCHR, &
!    !!!                FileFormat, &
!    !!!                FileType, &
!    !!!                Debug, &
!    !!!                VIsDouble)
!    !!!
!    !!!    !
!    !!!    !... Write the zone header information.
!    !!!    !
!    !!!    
!    !!!    I = TecZne142(trim(hgs.name)//NULLCHR, &
!    !!!                ZoneType, &
!    !!!                IMax, &
!    !!!                JMax, &
!    !!!                KMax, &
!    !!!                ICellMax, &
!    !!!                JCellMax, &
!    !!!                KCellMax, &
!    !!!                SolTime, &
!    !!!                StrandID, &
!    !!!                ParentZn, &
!    !!!                IsBlock, &
!    !!!                NFConns, &
!    !!!                FNMode, &
!    !!!                0, &
!    !!!                0, &
!    !!!                0, &
!    !!!                Null, &
!    !!!                Null, &
!    !!!                Null, &
!    !!!                ShrConn)
!    !!!    !
!    !!!    !... Write out the field data.
!    !!!    !
!    !!!     isDouble = 0
!    !!!    call Msg('write x')
!    !!!    I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.x),0)
!    !!!    call Msg('write y')
!    !!!    I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.y),0)
!    !!!    call Msg('write z')
!    !!!    I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.z),0)
!    !!!   
!    !!!    !isDouble = 0
!    !!!    !I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.x),0)
!    !!!    !I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.y),0)
!    !!!    !I   = TecDat142(hgs.mesh.NN,real(hgs.mesh.z),0)
!    !!!    
!    !!!    ! connectivity list
!    !!!    i1=0
!    !!!    if(hgs.mesh.nln==6) i1=2
!    !!!    allocate(NData(hgs.mesh.nln+i1,hgs.mesh.NE), stat=ialloc)
!    !!!    call AllocChk(ialloc,'ndata')
!    !!!
!    !!!    do i=1,hgs.mesh.NE
!    !!!        if(hgs.mesh.nln==8) then
!    !!!            do j=1,hgs.mesh.nln
!    !!!                NData(j,i)=hgs.mesh.in(j,i)
!    !!!            end do
!    !!!        else if(hgs.mesh.nln==6) then
!    !!!            do j=1,3
!    !!!                NData(j,i)=hgs.mesh.in(j,i)
!    !!!            end do
!    !!!            NData(4,i)=hgs.mesh.in(3,i)
!    !!!            do j=4,6
!    !!!                NData(j+1,i)=hgs.mesh.in(j,i)
!    !!!            end do
!    !!!            NData(8,i)=hgs.mesh.in(6,i)
!    !!!        else 
!    !!!            write(TMPStr,*) hgs.mesh.nln
!    !!!            call Msg('hgs.mesh.nln '//trim(TMPStr))
!    !!!            stop
!    !!!        end if
!    !!!    end do
!    !!!                
!    !!!    I   = TECNOD142(NData)
!    !!!
!    !!!   ! I   = TecDat142(hgs.mesh.ne,real(hgs.iprop),0)
!    !!!    
!    !!!    
!    !!!
!    !!!    I = TecEnd142()
!    !!!    
!    !!!end subroutine HGS_HGSDirToTecplot
!   
!   !------------------------------------------------------------------------
!   subroutine HGS_GatherObsWellFlowFiles(FnumTG)
!        ! For ...
!
!        implicit none
!
!        integer :: FnumTG
!
!        integer :: i, j
!
!
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FNumTecplot
!        character(MAXLBL) :: FNameTecplot
!        integer :: FNumTecplotBuild
!        character(MAXLBL) :: FNameTecplotBuild
!        
!        character(MAXLBL) :: ObsWellName
!        character(MAXLBL) :: LocalPrefix
!        character(MAXLBL) :: Path
!        character(MAXLBL) :: line
!        
!        logical :: DoneHeader
!        integer :: i1
!        
!        read(FNumTG,'(a)') ObsWellName
!        FName='scratchlist'
!        inquire(File=FName,Exist=FileExists)
!        if(FileExists) i=system('del scratchlist')
!        
!        TmpSTR='dir /s /b *.observation_well_flow.'//trim(ObsWellName)//'* > '//trim(FName)
!        i=system(TmpSTR)
!
!        call OpenAscii(FNum,FName)
!        call Msg( 'Obs well name: '//FName)
!        
!        FNameTecplotBuild=trim(ObsWellName)//'.gather.dat'
!        call OpenAscii(FNumTecplotBuild,FNameTecplotBuild)
!        call Msg( 'FNameTecplotBuild: '//FNameTecplotBuild)
!        
!        DoneHeader=.false.
!
!        FilesInList: do 
!        
!            read(FNum,'(a)',iostat=status) FNameTecplot
!            if(status/=0) then
!                call Msg( 'End of file list')
!                call FreeUnit(FNum)
!                exit FilesInList
!            end if
!
!            call OpenAscii(FNumTecplot,FNameTecplot)
!            call Msg( 'Tecplot file: '//FNameTecplot)
!            
!            ! determine prefix and path
!            i1=index(FNameTecplot,'o.observation_well_flow.')
!            do i=i1,1,-1
!                if(FNameTecplot(i:i)=='\' .or. FNameTecplot(i:i)=='/') exit
!            end do
!                
!            LocalPrefix=FNameTecplot(i+1:i1-1)
!            call Msg( 'Prefix: '//trim(LocalPrefix))
!                
!            if(i>1) then
!            do j=i-1,1,-1
!                if(FNameTecplot(j:j)=='\' .or. FNameTecplot(j:j)=='/') exit
!            end do
!                Path=FNameTecplot(j+1:i-1)
!            else
!                Path='No Path'
!            end if
!            call Msg( 'Path: '//trim(Path))
!                
!            if(.not. DoneHeader) then  
!                read(FNumTecplot,'(a)', iostat=status) line  ! title
!                write(FNumTecplotBuild,'(a)') trim(line)
!                
!                read(FNumTecplot,'(a)', iostat=status) line  ! variable
!                write(FNumTecplotBuild,'(a)') trim(line)
!                
!                if(index(line,'Time') > 0) then ! point
!                    call Msg( 'Observation point data type')
!                    read(FNumTecplot,'(a)') line   ! zone
!                    i1=index(line,'"')
!                    line=line(:i1)//trim(Path)//'"'
!                    write(FNumTecplotBuild,'(a)') trim(line)
!                else
!                    call Msg( 'Observation well data type (not supported)')
!                    call FreeUnit(FNumTecplot)
!                    exit FilesInList
!                end if
!                DoneHeader=.true.
!            else
!                read(FNumTecplot,'(a)') line  ! discard title 
!                read(FNumTecplot,'(a)') line  ! discard variable
!                read(FNumTecplot,'(a)') line   ! zone
!                i1=index(line,'"')
!                line=line(:i1)//trim(Path)//'"'
!                write(FNumTecplotBuild,'(a)') trim(line)
!            end if
!
!            
!            DataInFile: Do
!                read(FNumTecplot,'(a)', iostat=status) line  
!                if(status/=0) then
!                    call Msg( 'End of obs data')
!                    exit DataInFile
!                end if
!                write(FNumTecplotBuild,'(a)') trim(line)
!            end do DataInFile
!
!            
!            continue
!            
!            !do
!            !    write(FNum,frmt) i,',',trim(hgs.PMZonename(i)),',',ZoneECount(i),',',minKx(i),',',maxKx(i),',',minKy(i),',',maxKy(i),',',minKz(i),',',maxKz(i)            
!            !end do
!        
!        end do FilesInList
!        
!        
!        
!
!    end subroutine HGS_GatherObsWellFlowFiles
!   ! !------------------------------------------------------------------------
!   ! subroutine HGS_NodalSumByNodeList(FnumTG,hgs)
!   !     ! For 
!   !
!   !     implicit none
!   !
!   !     integer :: FnumTG
!   !
!   !     integer :: i, j
!   !
!   !     type (HGSProject) hgs
!   !
!   !	    real :: var(np)
!   !
!	  !  call getunit(itmp)
!	  !  open(itmp,file=fname,status='old',action = 'read',form='unformatted')
!	  !  if(status /= 0) then
!		 !   call Msg('read_var4 FILE ERROR: '//fname)
!		 !   stop
!	  !  end if
!	  !  read(itmp) message
!	  !  read(itmp) (var(j),j=1,np)
!	  !  call freeunit(itmp)
!   !end subroutine HGS_NodalSumByNodeList
!
!    !----------------------------------------------------------------------
!    subroutine find_node(x1,y1,z1,node,global)
!         
!        implicit none
!        type (HGSProject) global
!
!	    integer :: i
!	    integer :: node
!        real(dr) :: x1, y1, z1, dist_min, f1
!
!        dist_min=1.0e20
!	    do i=1,global.mesh.NN
!		    f1=sqrt((x1-global.mesh.X(i))**2+((y1-global.mesh.Y(i)))**2+((z1-global.mesh.Z(i)))**2)
!		    if(f1.lt.dist_min) then
!			    node=i
!			    dist_min=f1
!		    end if
!	    end do
!
!	    write(TMPStr,'(a14,3f17.5)') 'Target x, y, z ',x1,y1,z1
!        call Msg(TMPStr)
!	    write(TMPStr,'(a14,3f17.5)') 'Found x, y, z  ',global.mesh.X(node),Global.mesh.Y(node),Global.mesh.Z(node)
!        call Msg(TMPStr)
!	    write(TMPStr,'(a14,3f17.5)') 'Delta x, y, z  ',Global.mesh.X(node)-x1,Global.mesh.Y(node)-y1,Global.mesh.Z(node)-z1
!        call Msg(TMPStr)
!
!    end subroutine find_node
!    
    subroutine calc_3d_faces(global)  
        !use prepro_data
        !use faces
        implicit none
        type (HGSProject) global

	    if(Global.faces_calculated) return

	    if(Global.blockel) then
		    call calc_3d_faces_block(global)
	    else
		    call calc_3d_faces_prism(global)	!DB dec-06
	    end if

	    Global.faces_calculated = .true.

    end subroutine calc_3d_faces 

    !----------------------------------------------------------------------
    subroutine calc_3d_faces_block(global)
	    ! Define all possible 3D faces
        !use prepro_data
        !use faces
        implicit none
        type (HGSProject) global



	    integer, allocatable :: test_nval(:)
	    integer, allocatable :: indx_face(:)


	    integer, allocatable :: face_node_temp(:,:)
	    integer, allocatable :: face_el_temp(:,:)
	    integer, allocatable :: face_is_temp(:)
	    logical,allocatable :: dup(:)


	    integer :: isrt1(4), isrt2(4)

	    integer :: n_block_faces,i,j,i1,i2,nface_new

	
	    if(allocated(global.nf_nd)) deallocate(global.nf_nd,global.face_index)
	    allocate(global.nf_nd(global.mesh.nn),global.face_index(global.mesh.nn,80),stat=ialloc)
	    call AllocChk(ialloc,'calc_3d_faces_block arrays')
    !	Set nf_nd and global.face_index to zero. They will be set below and used later to find faces by node number
	    global.nf_nd(:) =0
	    global.face_index(:,:) =0

	    global.nf_cur=1
    !	call initialize_face_arrays(10000)

	    n_block_faces=n_block_faces_external
	    if(global.internal_faces) then
		    !tg-jun06
		    !n_block_faces=n_block_faces+n_block_faces_internal
		    n_block_faces=n_block_faces_external + n_block_4node_faces_internal + n_block_3node_faces_external + n_block_3node_faces_internal
	    end if


	    ! Define all possible faces
	    global.nface=0
        call initialize_face_arrays(global.mesh.ne*n_block_faces,global)   !rt 20080718
	    do i=1,global.mesh.ne
            do j=1,n_block_faces  
			    global.nface=global.nface+1
			    !call reallocate_face_arrays(nface) 
			
			    global.face_node(global.nface,1)=global.mesh.in(faceid_block(1,j),i)
			    global.face_node(global.nface,2)=global.mesh.in(faceid_block(2,j),i)
			    global.face_node(global.nface,3)=global.mesh.in(faceid_block(3,j),i)
			    if(faceid_block(4,j) > 0) then
				    global.face_node(global.nface,4)=global.mesh.in(faceid_block(4,j),i)
			    end if

			    global.face_el(global.nface,1)=i

			    ! for now, just flag the original 6 outer faces
			    if(j <= 6) call set(global.face_is(global.nface),j)

		    end do
        end do

	    allocate(test_nval(global.nface),indx_face(global.nface),dup(global.nface),stat=ialloc)
	    call AllocChk(ialloc,'face work arrays')

	    call Msg('calc_3d_faces_block')
	    call sort_faces(test_nval,indx_face,global)

	    ! remove duplicates
	    dup(:)=.false.
	    nface_new=global.nface

	    do i=1,global.nface-1
		    i1=indx_face(i)
		    i2=indx_face(i+1)

		    call sort_face(i1,isrt1,global)
		    call sort_face(i2,isrt2,global)

		    if(test_nval(i1)==test_nval(i2)) then !potential duplicate
			    if(global.face_node(i1,isrt1(1)) == global.face_node(i2,isrt2(1)) .and. global.face_node(i1,isrt1(2)) == global.face_node(i2,isrt2(2)) .and.  &
			       global.face_node(i1,isrt1(3)) == global.face_node(i2,isrt2(3)) .and. global.face_node(i1,isrt1(4)) == global.face_node(i2,isrt2(4))) then  ! check
				    dup(i2)=.true.
				    global.face_el(i1,2)=global.face_el(i2,1)
				    nface_new=nface_new-1
			    end if
		    end if
	    end do


	    ! save results global.mesh.in temporary arrays
	    allocate(face_node_temp(nface_new,4),face_el_temp(nface_new,2),face_is_temp(nface_new),stat=ialloc)
	    call AllocChk(ialloc,'face work arrays 2')

	    nface_new=0
	    do i=1,global.nface
		    if(.not. dup(i)) then 
			    nface_new=nface_new+1
			    face_node_temp(nface_new,1) = global.face_node(i,1)
			    face_node_temp(nface_new,2) = global.face_node(i,2)
			    face_node_temp(nface_new,3) = global.face_node(i,3)
			    face_node_temp(nface_new,4) = global.face_node(i,4)
			    face_el_temp(nface_new,1)=global.face_el(i,1)
			    face_el_temp(nface_new,2)=global.face_el(i,2)
			    face_is_temp(nface_new)=global.face_is(i)

			    i1=minval(face_node_temp(nface_new,:),MASK=face_node_temp(nface_new,:).gt.0)
			    global.nf_nd(i1)=global.nf_nd(i1)+1
	 		    !call user_size_check(global.nf_nd(i1),user_face_nd,user_face_nd_str)
			    global.face_index(i1,global.nf_nd(i1))=nface_new
		    end if
	    end do

	    deallocate(dup)

	    ! reallocate arrays
	    global.nface=nface_new
	    deallocate(global.face_node,global.face_el,global.face_is)
	    allocate(global.face_node(global.nface,4),global.face_el(global.nface,2),global.face_is(global.nface),stat=ialloc)
	    call AllocChk(ialloc,'final face arrays')
	    global.face_node(:,:)=face_node_temp(:,:)
	    global.face_el(:,:)=face_el_temp(:,:)
	    global.face_is(:)=face_is_temp(:)

	    deallocate(test_nval,indx_face,face_node_temp, face_is_temp, face_el_temp)

	    !if(overland_flow) then
		   ! if(allocated(face_olf_el)) deallocate(face_olf_el)
		   ! allocate(face_olf_el(global.nface),stat=ialloc)
		   ! call AllocChk(ialloc,'calc_3d_face overland flow array')
		   ! face_olf_el(: ) =0
	    !end if
    end subroutine calc_3d_faces_block

!----------------------------------------------------------------------
subroutine calc_3d_faces_prism(global)
	! set up all possible 3D faces
    !use prepro_data
    use faces
    implicit none
    type (HGSProject) global


	integer, allocatable :: test_nval(:)
	integer, allocatable :: indx_face(:)

	integer, allocatable :: face_node_temp(:,:)
	integer, allocatable :: face_el_temp(:,:)
	integer, allocatable :: face_is_temp(:)
	logical,allocatable :: dup(:)
    
    integer, allocatable :: el_list(:,:)
    integer, allocatable :: el_listSize(:)
    integer :: eCur, eConn



	integer :: isrt1(4), isrt2(4)

	integer :: n_prism_faces,i,j,i1,i2,nface_new

	if(allocated(global.nf_nd)) deallocate(global.nf_nd,global.face_index)
	allocate(global.nf_nd(global.mesh.nn),global.face_index(global.mesh.nn,80),stat=ialloc)
	call AllocChk(ialloc,'calc_3d_faces_prism arrays')
!	Set global.nf_nd and global.face_index to zero. They will be set below and used later to find faces by node number
	global.nf_nd(:) =0
	global.face_index(:,:) =0

	global.nf_cur=1
!	call initialize_face_arrays(10000)


	n_prism_faces=n_prism_faces_external
	if(global.internal_faces) then
		n_prism_faces=n_prism_faces+n_prism_faces_internal
	end if


	! set up all possible faces
	global.nface=0
    call initialize_face_arrays(global.mesh.ne*n_prism_faces,global)
	do i=1,global.mesh.ne
        do j=1,n_prism_faces  
			global.nface=global.nface+1
			! call reallocate_face_arrays(global.nface) 
			
			global.face_node(global.nface,1)=global.mesh.in(faceid_prism(1,j),i)
			global.face_node(global.nface,2)=global.mesh.in(faceid_prism(2,j),i)
			global.face_node(global.nface,3)=global.mesh.in(faceid_prism(3,j),i)
			if(faceid_prism(4,j) > 0) then
				global.face_node(global.nface,4)=global.mesh.in(faceid_prism(4,j),i)
			end if

			global.face_el(global.nface,1)=i

			! for now, just flag the original 6 outer faces
			if(j <= 6) call set(global.face_is(global.nface),j)

		end do
    end do

	allocate(test_nval(global.nface),indx_face(global.nface),dup(global.nface),stat=ialloc)
	call AllocChk(ialloc,'face work arrays')

	call sort_faces(test_nval,indx_face,global)

	! remove duplicates
	dup(:)=.false.
	nface_new=global.nface

	do i=1,global.nface-1
		i1=indx_face(i)
		i2=indx_face(i+1)

		call sort_face(i1,isrt1,global)
		call sort_face(i2,isrt2,global)

		if(test_nval(i1)==test_nval(i2)) then !potential duplicate
			if(global.face_node(i1,isrt1(1)) == global.face_node(i2,isrt2(1)) .and. global.face_node(i1,isrt1(2)) == global.face_node(i2,isrt2(2)) .and.  &
			   global.face_node(i1,isrt1(3)) == global.face_node(i2,isrt2(3)) .and. global.face_node(i1,isrt1(4)) == global.face_node(i2,isrt2(4))) then  ! check
				dup(i2)=.true.
				global.face_el(i1,2)=global.face_el(i2,1)
				nface_new=nface_new-1
			end if
		end if
	end do


	! save results global.mesh.in temporary arrays
	allocate(face_node_temp(nface_new,4),face_el_temp(nface_new,2),face_is_temp(nface_new),stat=ialloc)
	call AllocChk(ialloc,'face work arrays 2')

	nface_new=0
	do i=1,global.nface
		if(.not. dup(i)) then 
			nface_new=nface_new+1
			face_node_temp(nface_new,1) = global.face_node(i,1)
			face_node_temp(nface_new,2) = global.face_node(i,2)
			face_node_temp(nface_new,3) = global.face_node(i,3)
			face_node_temp(nface_new,4) = global.face_node(i,4)
			face_el_temp(nface_new,1)=global.face_el(i,1)
			face_el_temp(nface_new,2)=global.face_el(i,2)
			face_is_temp(nface_new)=global.face_is(i)

			i1=minval(face_node_temp(nface_new,:),MASK=face_node_temp(nface_new,:).gt.0)
			global.nf_nd(i1)=global.nf_nd(i1)+1
	 		!call user_size_check(global.nf_nd(i1),user_face_nd,user_face_nd_str)
			global.face_index(i1,global.nf_nd(i1))=nface_new
		end if
	end do

	deallocate(dup)

	! reallocate arrays
	global.nface=nface_new
	deallocate(global.face_node,global.face_el,global.face_is)
	allocate(global.face_node(global.nface,4),global.face_el(global.nface,2),global.face_is(global.nface),stat=ialloc)
	call AllocChk(ialloc,'final face arrays')
	global.face_node(:,:)=face_node_temp(:,:)
	global.face_el(:,:)=face_el_temp(:,:)
	global.face_is(:)=face_is_temp(:)

	deallocate(test_nval,indx_face,face_node_temp, face_is_temp, face_el_temp)

	!if(overland_flow) then
	!	if(allocated(face_olf_el)) deallocate(face_olf_el)
	!	allocate(face_olf_el(global.nface),stat=ialloc)
	!	call AllocChk(ialloc,'calc_3d_face overland flow array')
	!	face_olf_el(: ) =0
	!end if
    
    ! Form element connection list from face neighbour elements
    allocate(el_list(global.mesh.ne,10),el_listSize(global.mesh.ne),stat=ialloc)
	call AllocChk(ialloc,'element connection list arrays')
    el_list(:,:)=0
    el_listSize(:)=0
    
    do i=1,global.nface
        eCur=global.face_el(i,1)
        eConn=global.face_el(i,2)
        if(eConn /= 0) then
            el_listSize(eCur)=el_listSize(eCur)+1
            el_list(eCur,el_listSize(eCur))=eConn
            if(i<100) write(*,*) ecur, econn, el_listsize(eCur)
        end if
    end do
        

end subroutine calc_3d_faces_prism

!----------------------------------------------------------------------
subroutine initialize_face_arrays(proposed,global)
	!use prepro_data
	implicit none
    type (HGSProject) global

	integer :: proposed

	if(proposed > global.nf_cur) global.nf_cur=proposed

	if(allocated(global.face_node)) then
		deallocate(global.face_node, global.face_el, global.face_is)
	end if

	allocate(global.face_node(global.nf_cur,4), global.face_el(global.nf_cur,2), global.face_is(global.nf_cur),stat=ialloc)
	call AllocChk(ialloc,'initial face arrays')
    global.face_node(:,:)=0
    global.face_el(:,:)=0
	global.face_is(:)=0

end subroutine initialize_face_arrays
!----------------------------------------------------------------------
subroutine sort_face(ifc,isrt,global)
    !use prepro_data
    implicit none
    type (HGSProject) global

	integer :: j, ifc, n_srt(4), isrt(4)

	do j=1,4
		n_srt(j)=global.face_node(ifc,j)
	end do

	if(n_srt(4)==0) n_srt(4)=global.mesh.nn+1

	call indexx_int(4,n_srt,isrt)

end subroutine sort_face
!----------------------------------------------------------------------
subroutine sort_faces(test,indx,global)
    !use prepro_data
    implicit none
    type (HGSProject) global

    integer :: i, itemp, i1, i2
	integer :: isrt1(4), isrt2(4)

	integer :: test(global.nface), indx(global.nface)

	! sort face nodes 
	do i=1,global.nface
		call sort_face(i,isrt1,global)
		test(i)=global.face_node(i,isrt1(1))  ! store min node number as test
	end do
	
	call indexx_int(global.nface,test,indx) 


	! order within test_val groups by isrt(2)
	i=1
	do while (i < global.nface)
		i1=indx(i)
		i2=indx(i+1)

		call sort_face(i1,isrt1,global)
		call sort_face(i2,isrt2,global)

		if(test(i1)==test(i2)) then 

			if(global.face_node(i1,isrt1(2)) > global.face_node(i2,isrt2(2))) then  ! swap values
				itemp=i2
				indx(i+1)=i1
				indx(i)=itemp

				itemp=test(i2)
				test(i2)=test(i1)
				test(i1)=itemp
				
				if(i>1) i=i-1
			else
				i=i+1
			end if
		else
			i=i+1
		end if
	end do


	! order within test_val groups by isrt(3)
	i=1
	do while (i < global.nface)
		i1=indx(i)
		i2=indx(i+1)

		call sort_face(i1,isrt1,global)
		call sort_face(i2,isrt2,global)

		if(test(i1)==test(i2) .and.  &
			global.face_node(i1,isrt1(2)) == global.face_node(i2,isrt2(2))) then 

			if(global.face_node(i1,isrt1(3)) > global.face_node(i2,isrt2(3))) then  ! swap values

				itemp=i2
				indx(i+1)=i1
				indx(i)=itemp

				itemp=test(i2)
				test(i2)=test(i1)
				test(i1)=itemp
				
				
				if(i>1) i=i-1
			else
				i=i+1
			end if
		else
			i=i+1
		end if
	end do

	! order within test_val groups by isrt(4)
	i=1
	do while (i < global.nface)
		i1=indx(i)
		i2=indx(i+1)

		call sort_face(i1,isrt1,global)
		call sort_face(i2,isrt2,global)

		if(test(i1)==test(i2) .and.  &
			global.face_node(i1,isrt1(2)) == global.face_node(i2,isrt2(2)) .and. &
			global.face_node(i1,isrt1(3)) == global.face_node(i2,isrt2(3))) then 

			if(global.face_node(i1,isrt1(4)) > global.face_node(i2,isrt2(4))) then  ! swap values

				itemp=i2
				indx(i+1)=i1
				indx(i)=itemp

				itemp=test(i2)
				test(i2)=test(i1)
				test(i1)=itemp
				
				if(i>1) i=i-1
			else
				i=i+1
			end if
		else
			i=i+1
		end if
	end do

end subroutine sort_faces
!
!!----------------------------------------------------------------------
!    subroutine HGS_FracFromNodeList(FnumTG, hgs)  
!        implicit none
!        
!        type(HGSProject) hgs
!    
!        integer :: i, j 
!        integer :: nfe_old
!        integer :: nnfrac_old
!        
!        integer :: FnumTG
!    
!        integer :: FNumNList
!        character(MAXLBL) :: FNAMENodeList   
!        
!        integer :: iNd
!       
!	    integer :: ncount
!    
!	    integer :: ncount_frac
!        
!        real(dr) :: ApDefault
!
!        ! clear chosen node set each time through
!        do i=1,hgs.mesh.nn
!            call clear(hgs.node_is(i),chosen)
!        end do
!        
!        ! read node set and set chosen nodes
!        read(FnumTG,'(a)') FNAMENodeList
!        call OpenAscii(FNumNList,FNAMENodeList)
!        call Msg( 'From File: '//FNAMENodeList)
!        do 
!            read(FNumNList,*,iostat=status) iNd
!            if(status/=0) exit
!            call set(hgs.node_is(iNd),chosen)
!        end do 
!        
!        ! need to dimension these arrays
!        hgs.mesh.NLNF=4
!        if(.not. allocated(hgs.mesh.inf)) then  ! no fractures yet
!            
!            ! Coupling scheme  0 common, 1 multi
!            read(FnumTG,*) hgs.CplScheme
!            if(hgs.CplScheme==Multi) then
!                call Msg( 'Dual node coupling scheme')
!            else
!                call Msg( 'Common node coupling scheme')
!            end if
!         
!
!            hgs.mesh.nef=0
!            
!            !  Nodal fracture arrays 
!            allocate(hgs.head(hgs.mesh.nn),hgs.mesh.inf(hgs.mesh.NLNF,maxnef), stat=ialloc)
!            call AllocChk(ialloc,'HGS fracture incidences')
!            hgs.mesh.inf(:,:)=0
!
!            !  Elemental fracture arrays initially set to size maxnef
!            allocate(hgs.ap(maxnef), hgs.ifrac_id_elem(maxnef),  hgs.ifrac_3d_elem_map(maxnef,2), stat=ialloc)
!            call AllocChk(ialloc,'HGS fracture element arrays')
!            hgs.ap(:)=0.0d0
!            hgs.ifrac_id_elem(:)=0.0d0
!            hgs.ifrac_3d_elem_map(:,:)=0.0d0
!            hgs.nzones_fprop=0
!
!        end if
!
!         
!
!        ApDefault=0.001  ! i.e. 1 millimetre
!        write(TmpSTR,'(a,e18.5)')     'default aperture (m):              ',ApDefault
!        call Msg(trim(TMPStr))
!
!        nfe_old=hgs.mesh.nef
!        nnfrac_old=hgs.nnfrac
!        hgs.nzones_fprop=hgs.nzones_fprop+1
!                
!	    element: do i=1,hgs.nface
!   
!		    ncount=0
!		    do j=1,4
!			    if(hgs.face_node(i,j)/=0) then
!                    if(bcheck(hgs.node_is(hgs.face_node(i,j)),chosen)) then
!				        ncount=ncount+1
!                    end if
!                end if
!            end do
!        
!            if(hgs.face_node(i,4)/=0 .and. ncount==4) then   ! rectangular fracture element
!			    hgs.mesh.nef=hgs.mesh.nef+1
!                hgs.ap(hgs.mesh.nef)=ApDefault
!                hgs.ifrac_id_elem(hgs.mesh.nef)=hgs.nzones_fprop
!    
!                do j=1,4
!				    hgs.mesh.inf(j,hgs.mesh.nef)=hgs.face_node(i,j)
! 				    call set(hgs.node_is(hgs.mesh.inf(j,hgs.mesh.nef)),a_frac)
!			    end do
!                
!            else if (hgs.face_node(i,4)==0 .and. ncount==3) then   ! triangular fracture element    
!			    hgs.mesh.nef=hgs.mesh.nef+1
!                hgs.ap(hgs.mesh.nef)=ApDefault
!                hgs.ifrac_id_elem(hgs.mesh.nef)=hgs.nzones_fprop
!                do j=1,3
!				    hgs.mesh.inf(j,hgs.mesh.nef)=hgs.face_node(i,j)
! 				    call set(hgs.node_is(hgs.mesh.inf(j,hgs.mesh.nef)),a_frac)
!			    end do
!            end if
!    
!        end do element
!        
!        ! debug: link_frac2pm needs to be dimensioned to total number of fracture nodes.
!        ! ncount_frac currently has number of fracture nodes added from this list
!        
!        
!        ! Count defined fracture nodes at each pass 
!	    hgs.nnfrac = 0
!	    do i=1,hgs.mesh.nn
!		    if(bcheck(hgs.node_is(i),a_frac)) then
!			    hgs.nnfrac = hgs.nnfrac + 1
!		    end if
!        end do
!        
!        
!        
!        if(allocated(hgs.link_frac2pm)) deallocate(hgs.link_frac2pm,hgs.link_pm2frac)
!        allocate(hgs.link_frac2pm(hgs.nnfrac),hgs.link_pm2frac(hgs.mesh.nn),stat=ialloc)
!	    call AllocChk(ialloc,'HGS Fracture linking arrays')
!	    hgs.link_frac2pm = 0 ! automatic initialization
!	    hgs.link_pm2frac(: ) = 0
!        ncount_frac=0
!        do i=1,hgs.mesh.nn
!		    if(bcheck(hgs.node_is(i),a_frac)) then
!			    ncount_frac = ncount_frac + 1
!			    hgs.link_frac2pm(ncount_frac) = i
!			    if(hgs.CplScheme==Multi) then
!                    hgs.link_pm2frac(i) = hgs.mesh.nn+hgs.nnfrac ! +nndual+nnolf
!                else
!                    hgs.link_pm2frac(i) = hgs.mesh.nn  ! needs to be verified
!                end if
!		    end if
!        end do
!    
!
!        call Msg('HGS Summary:')
!        write(TmpSTR,'(a,i10)')     'New fracture elements:  ',hgs.mesh.nef-nfe_old
!        call Msg(trim(TMPStr))
!        write(TmpSTR,'(a,i10)')     'Total fracture elements now:  ',hgs.mesh.nef
!        call Msg(trim(TMPStr))
!        !hgs.name="NodeSet"
!    
!      
!    
!    end subroutine HGS_FracFromNodeList

    

end module HGS

     !----------------------------------------------------------------------
double precision function power(base_,exponent_)
    !.....................................................................
    !
    ! AUTHOR		Thomas Graf
    !
    ! AFFILIATION	Université Laval
    !
    ! DATE			July 6, 2004
    !
    ! DESCRIPTION	Raises the base to the exponent
    !
    ! LITERATURE	--
    !
    ! INPUT			base, exponent						double precision numbers
    !
    ! OUTPUT		base^exponent						double precision number
    !
    !.....................................................................

    use GeneralRoutines
    implicit none

    real(dr)			:: base_,exponent_
    real(dr)			:: base,exponent
    real(dr), parameter	:: cTiny = 1.0d-15

    power = 0.0d0
    base = base_
    exponent = exponent_

    ! Deal with the near zero special cases
    if(abs(base) < cTiny) then
	    base = 0.0d0
    end if
    if(abs(exponent) < cTiny) then
	    exponent = 0.0d0
    end if

    ! Deal with the exactly zero cases
    if(base == 0.0d0) then
	    power = 0.0d0
    end if
    if(exponent == 0.0d0) then
	    power = 1.0d0
    end if

    ! Cover everything else
    if((base < 0.0d0) .and. (exponent < 0.0d0)) then
        power = 1/dexp(-exponent*log(-base))
    elseif((base < 0.0d0) .and. (exponent >= 0.0d0)) then
        power = dexp(exponent*log(-base))
    elseif((base > 0.0d0) .and. (exponent < 0.0d0)) then
        power = 1.0d0/dexp(-exponent*log(base))
    elseif((base > 0.0d0) .and. (exponent >= 0.0d0)) then
        power = dexp(exponent*log(base))
    end if

    ! Correct the sign
    if ((base < 0.0d0) .and. (mod(exponent,2.0d0) /= 0.0d0)) then
      power = -power
    end if

    return
end function power
