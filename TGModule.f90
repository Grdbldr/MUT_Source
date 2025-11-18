module MUT  !### Modflow-USG Tools
    use GeneralRoutines
    use error_param
    use MeshGen
    use gb
    use MUSG
    use tecplot
    use NumericalMesh
    
    implicit none

    character(MAX_INST) :: MUT_CMD="none"
    character(MAX_INST) :: BuildModflowUSG_CMD="build modflow usg"
    character(MAX_INST) :: PostprocessExistingModflowModel_CMD="postprocess existing modflow model"

    character(256) :: FileNameMUT
    integer(i4) :: FnumMUT
    integer(i4) :: FnumUserMUT
    character(40) :: prefix = ''
    integer(i4)	:: l_prfx  = 0
    
    character(MAX_LBL) :: DirName ! directory name

    contains

    subroutine Header
        call date_and_time(DateSTR, TIME = TimeSTR, ZONE = TimezoneSTR)
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@                                                                      @@ ')
        call Msg( '@@                    MUT         '//MUTVersion//'@@ ')
        call Msg( '@@                    Run date '//DateStr//'                             @@ ')
        call Msg( '@@                                                                      @@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
    end subroutine  header

    subroutine OpenMUT  !--- Modflow user tools  .mut

        write(*,'(a)')  'MUT version '//MUTVersion
        
        #ifdef _DEBUG   
            call openascii(iDBG,FNameDBG)
        #endif


        ! open the user MUT input file
        call EnterPrefix(prefix,l_prfx,FnumUserMUT,'mut')
        
        CmdLine=' del '//trim(prefix(:l_prfx))//'o.*'
        CALL execute_command_line(trim(CmdLine))

        ! open a file called prefix.eco, if it exists, overwrite it with MUT header
        FNameEco=prefix(:l_prfx)//'o.eco'
        call openascii(FnumEco,FNameEco)
        call header
        call Msg(' ')
        call Msg (FileReadSTR//'User input file: '//prefix(:l_prfx)//'.mut')
        call Msg(' ')
        call Msg (FileCreateSTR//'Echo file: '//FNameEco)
        ErrFNum=FnumEco


        ! Create one processed input file
        FNameInput=prefix(:l_prfx)//'o.input'
        call openascii(FnumMUT,FNameInput)

        ! strip out blanks and comments and concatenate included files
        call StripComments(FnumUserMUT,FnumMUT)
	    call freeunit(FnumUserMUT)
	    call Msg(' ')
        call Msg (FileCreateSTR//'Stripped input file: '//FNameInput)

    end subroutine OpenMUT



    subroutine ProcessMUT !--- Command processor for Modflow-USG Tools (.mut file extension)

        type (ModflowProject) MyProject
        
        type(MeshGroup) MyMeshGroup
        !type (HGSProject) MyHGS
        
        ! Ways to define the 2D template mesh
        character(MAX_INST) :: MeshFromGb_CMD          =   '2d mesh from gb'
        character(MAX_INST) :: QuadtreeMeshFromGWV_CMD =   '2d quadtree mesh from groundwater vistas'
        character(MAX_INST) :: GenerateUniformRectangles_CMD  =   'generate uniform rectangles'
        character(MAX_INST) :: GenerateVariableRectangles_CMD  =   'generate variable rectangles'
        character(MAX_INST) :: GenerateSegmentsFromXYZEndpoints_CMD  =   'generate segments from xyz endpoints'
        character(MAX_INST) :: ReadMesh_CMD  =   'read mesh'
        
        ! There are many other possible 2d mesh definition options e.g.
        !character(MAX_INST), parameter :: g_rects_i           =   'generate rectangles interactive' 
        
                
        ! Ways to define a GridBuilder mesh
        character(MAX_INST) :: GridBuilder_CMD          =   'build triangular mesh'
        
        integer(i4) :: ierror

        do
            read(FnumMUT,'(a)',iostat=status,end=10) MUT_CMD
            call LwrCse(MUT_CMD)
            call Msg(' ')
            call Msg(MUT_CMD)


            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.mut'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
           end if

            if(index(MUT_CMD, StopWatch_CMD) /= 0) then
                read(FnumMUT,*) l1
                read(FnumMUT,'(a)') TmpSTR
                call StopWatch(l1,TmpSTR(:len_trim(TmpSTR)))
            else if(index(MUT_CMD, SplitTime_CMD) /= 0) then
                read(FnumMUT,*) l1
                call SplitTime(l1)
            else if(index(MUT_CMD, ElapsedTime_CMD) /= 0) then
                read(FnumMUT,*) l1
                call ElapsedTime(l1)
                
            ! Mesh generation options
            else if(index(MUT_CMD, MeshFromGb_CMD)  /= 0) then
                NeedGBName=.true.
                MyMeshGroup%nMesh=MyMeshGroup%nMesh+1
                call GrowMeshArray(MyMeshGroup%Mesh,MyMeshGroup%nMesh-1,MyMeshGroup%nMesh)
                call ReadGridBuilderMesh(FNumMut,MyMeshGroup%Mesh(MyMeshGroup%nMesh))                
                !TMPLT.Name='TMPLT'
                !!call MeshFromGb(FnumMUT,TMPLT)
                !call TemplateBuild(Modflow,TMPLT) ! Determine TMPLT cell connections (mc or nc), boundary nodes
            
            else if(index(MUT_CMD, GenerateUniformRectangles_CMD)  /= 0) then
                ! Build the 2D template mesh from a uniform 2D rectangular mesh
                MyMeshGroup%nMesh=MyMeshGroup%nMesh+1
                call GrowMeshArray(MyMeshGroup%Mesh,MyMeshGroup%nMesh-1,MyMeshGroup%nMesh)
                call GenerateUniformRectangles(FnumMUT,MyMeshGroup%Mesh(MyMeshGroup%nMesh))
                
                
                
                
            !    call TemplateBuild(Modflow,TMPLT) ! Determine TMPLT cell connections (mc or nc), boundary nodes
            !    JustBuilt=.true.
            !
            !else if(index(MUT_CMD, GenerateVariableRectangles_CMD)  /= 0) then
            !    ! Build the 2D template mesh from a variable 2D rectangular mesh
            !    call GenerateVariableRectangles(FnumMUT,TMPLT)
            !    call TemplateBuild(Modflow,TMPLT) ! Determine TMPLT cell connections (mc or nc), boundary nodes
            !    JustBuilt=.true.
                
            else if(index(MUT_CMD, GenerateSegmentsFromXYZEndpoints_CMD)  /= 0) then
                ! Build the 2D template mesh from a uniform 2D rectangular mesh
                MyMeshGroup%nMesh=MyMeshGroup%nMesh+1
                call GrowMeshArray(MyMeshGroup%Mesh,MyMeshGroup%nMesh-1,MyMeshGroup%nMesh)
                call GenerateSegmentsFromXYZEndpoints(FnumMUT,MyMeshGroup%Mesh(MyMeshGroup%nMesh))

            
            !else if(index(MUT_CMD, QuadtreeMeshFromGWV_CMD)  /= 0) then
            !    ! Build the 2D template mesh from a grdbldr 2D mesh
            !    call Quadtree2DMeshFromGWV(FnumMUT,TMPLT)
            !    call TemplateBuild(Modflow,TMPLT) ! Determine TMPLT cell connections (mc or nc), boundary nodes
           
                
            ! GridBuilder options
            else if(index(MUT_CMD, GridBuilder_CMD) /= 0) then
                MyMeshGroup%nMesh=MyMeshGroup%nMesh+1
                call GrowMeshArray(MyMeshGroup%Mesh,MyMeshGroup%nMesh-1,MyMeshGroup%nMesh)
                read(FNumMut,'(a80)') TmpSTR
                MyMeshGroup%mesh(MyMeshGroup%nMesh)%Name=TmpSTR
                call Msg('New mesh name: '//trim(MyMeshGroup%mesh(MyMeshGroup%nMesh)%Name))
                call GridBuilder(FNumMUT,MyMeshGroup%mesh(MyMeshGroup%nMesh),iError)
                call TriangularElementProperties(MyMeshGroup%mesh(MyMeshGroup%nMesh))
                ! These routines required for both node- and mesh-centred control volume cases
                call BuildFaceTopologyFrommesh(MyMeshGroup%mesh(MyMeshGroup%nMesh))  
                call FlagOuterBoundaryNodes(MyMeshGroup%mesh(MyMeshGroup%nMesh)) ! From faces connected to only 1 element 
                call BuildMeshCentredIaJa(MyMeshGroup%mesh(MyMeshGroup%nMesh)) 
                call SaveMeshBIN(MyMeshGroup%mesh(MyMeshGroup%nMesh))
                !call SaveMeshTIN(MyMeshGroup%mesh(MyMeshGroup%nMesh))
                if(EnableTecplotOutput) then
                    call MeshToTecplot(MyMeshGroup%mesh(MyMeshGroup%nMesh))
                endif
                if(EnableQGISOutput) then
                    call MeshToQGIS(MyMeshGroup%mesh(MyMeshGroup%nMesh))
                endif
                


            else if(index(MUT_CMD, ReadMesh_CMD) /= 0) then
                MyMeshGroup%nMesh=MyMeshGroup%nMesh+1
                call GrowMeshArray(MyMeshGroup%Mesh,MyMeshGroup%nMesh-1,MyMeshGroup%nMesh)
                read(FNumMut,'(a80)') TMPStr 
                MyMeshGroup%mesh(MyMeshGroup%nMesh)%Name=TMPStr
                call ReadMeshBIN(MyMeshGroup%mesh(MyMeshGroup%nMesh))

               
                continue


            ! Modflow options
            else if(index(MUT_CMD, BuildModflowUSG_CMD) /= 0) then
                call BuildModflowUSG(FnumMUT,MyProject,prefix)

            else if(index(MUT_CMD, PostprocessExistingModflowModel_CMD) /= 0) then
                call PostprocessExistingModflowModel(FnumMUT,MyProject,prefix)

            else
                call ErrMsg('MUT?:'//MUT_CMD)
            end if
        end do

        10 continue
    end subroutine ProcessMUT


    subroutine CloseMUT !--- Modflow-USG Tools .mut
       call Msg(' ')
       call Msg('Normal exit')
       call FreeUnit(FnumMUT)
    end subroutine CloseMUT



end module MUT
