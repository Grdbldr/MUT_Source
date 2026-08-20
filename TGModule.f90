module MUT  !### Modflow-USG Tools
    use GeneralRoutines
    use error_param
    use MeshGen
    use gb
    use MUSG
    use tecplot
    use NumericalMesh
    use ErrorHandling, only: ERR_FILE_IO, ERR_INVALID_INPUT, HandleError
    
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
    
    type MeshGroup
        integer (i4) :: nMesh=0
        type(mesh), allocatable :: mesh(:) ! array of meshes
    end type MeshGroup


    contains

    subroutine Header
        call date_and_time(DateSTR, TIME = TimeSTR, ZONE = TimezoneSTR)
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
        call Msg( '@@                                                @@')
        write(TMPStr,'(T1,a,T5,a,T20,a,t51,a)') '@@','MUT version:',trim(MUTVersion),'@@'
        call Msg(TMPStr)
        write(TMPStr,'(T1,a,T5,a,T20,a,t51,a)') '@@','Run date:',trim(DateStr),'@@'
        call Msg(TMPStr)
        call Msg( '@@                                                @@')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
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
                call HandleError(ERR_FILE_IO, 'Error reading file', 'read_instructions')
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
                NeedMeshName=.true.
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
                NeedMeshName=.true.
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
                NeedMeshName=.true.
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
            else if(index(MUT_CMD, 'write ascii tecplot output') /= 0) then
                WriteAsciiTecplot = .true.
                call Msg('*** Tecplot ASCII .dat output enabled for FE mesh/results')

            else if(index(MUT_CMD, 'no model documentation') /= 0) then
                WriteModelDocumentation = .false.
                call Msg('*** Model documentation (Docs/) disabled')

            else if(index(MUT_CMD, BuildModflowUSG_CMD) /= 0) then
                call BuildModflowUSG(FnumMUT,MyProject,prefix)
                if(WriteModelDocumentation) call WriteModelDossier()

            else if(index(MUT_CMD, PostprocessExistingModflowModel_CMD) /= 0) then
                call PostprocessExistingModflowModel(FnumMUT,MyProject,prefix)
                if(WriteModelDocumentation) call WriteModelDossier()

            else
                call HandleError(ERR_INVALID_INPUT, 'Unrecognized instruction: '//trim(MUT_CMD), 'read_instructions')
            end if
        end do

        10 continue
    end subroutine ProcessMUT


    subroutine CloseMUT !--- Modflow-USG Tools .mut
       call Msg(' ')
       call Msg('Normal exit')
       call FreeUnit(FnumMUT)
    end subroutine CloseMUT


    subroutine WriteModelDossier()
        ! After a successful _build or _post, run mut_document.py --skip-export.
        ! Missing Python, script, or pdflatex must not fail the MUT run.
        implicit none

        character(MAX_STR) :: arg0, exe_dir, userbin_dir, script, cmd
        integer(i4) :: i, n, cmdstat, exitstat
        logical :: script_exist, ran

        if(.not. WriteModelDocumentation) return

        script = ' '
        arg0 = ' '
        call get_command_argument(0, arg0)
        n = len_trim(arg0)
        if(n >= 2) then
            if(arg0(1:1) == '"' .and. arg0(n:n) == '"') then
                arg0 = arg0(2:n-1)
                n = len_trim(arg0)
            end if
        end if
        exe_dir = ' '
        do i = n, 1, -1
            if(arg0(i:i) == '\' .or. arg0(i:i) == '/') then
                exe_dir = arg0(1:i-1)
                exit
            end if
        end do
        if(len_trim(exe_dir) > 0) then
            script = trim(exe_dir)//'\mut_document\mut_document.py'
            inquire(file=trim(script), exist=script_exist)
            if(.not. script_exist) script = ' '
        end if
        ! Prefer %USERBIN% (where post_build deploys mut_document). Do not use
        ! DefineUserbin here: "use local databases" redirects that to a project
        ! folder that only has SMS/material CSVs.
        if(len_trim(script) == 0) then
            userbin_dir = ' '
            call GET_ENVIRONMENT_VARIABLE('USERBIN', userbin_dir)
            if(len_trim(userbin_dir) > 0) then
                script = trim(userbin_dir)//'\mut_document\mut_document.py'
                inquire(file=trim(script), exist=script_exist)
                if(.not. script_exist) script = ' '
            end if
        end if
        if(len_trim(script) == 0) then
            call WarnMsg('model documentation: mut_document.py not found next to mut.exe or %USERBIN%')
            return
        end if

        call Msg('Writing model documentation under Docs/ (PNG export skipped)')
        ran = .false.
        cmd = 'python "'//trim(script)//'" --skip-export .'
        cmdstat = -1
        exitstat = -1
        call execute_command_line(trim(cmd), wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        ! cmdstat/=0 or Windows 9009: python not on PATH; try the py launcher
        if(cmdstat /= 0 .or. exitstat == 9009) then
            cmd = 'py "'//trim(script)//'" --skip-export .'
            cmdstat = -1
            exitstat = -1
            call execute_command_line(trim(cmd), wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        end if
        if(cmdstat == 0) ran = .true.

        if(.not. ran) then
            call WarnMsg('model documentation: python/py not found; Docs/ was not updated')
            return
        end if
        if(exitstat /= 0) then
            call WarnMsg('model documentation: mut_document.py finished with a non-zero status (PDF or layouts may be incomplete)')
            return
        end if
        call Msg('Model documentation written under Docs/')
    end subroutine WriteModelDossier



end module MUT
