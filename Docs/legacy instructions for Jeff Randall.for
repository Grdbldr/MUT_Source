    ! legacy instructions for Jeff Randall
    character(60) :: MUSG_ReadAsciiHeadFile_CMD             ='read usgbin2tab_h head file'
    character(60) :: MUSG_ReadAsciiKxFile_CMD               ='read ascii kx file'
    character(60) :: MUSG_ReadAsciiSsFile_CMD               ='read ascii ss file'
    character(60) :: MUSG_ReadAsciiSyFile_CMD               ='read ascii sy file'
    character(60) :: MUSG_ReadAsciiVanisFile_CMD            ='read ascii vanis file'
    character(60) :: MUSG_ReadWellConstructionCSVFile_CMD   ='read well construction csv file'
    character(60) :: MUSG_Read_EIWellCSVFile_CMD            ='read ei well construction csv file'
    character(60) :: MUSG_ReadRiverFlowsAsciiFile_CMD       ='read river cell flows'
    character(60) :: MUSG_RiverFlowsToTecplot_CMD           ='modflow-usg river flows to tecplot'
    character(60) :: MUSG_ReadHeadCalibrationAsciiFile_CMD  ='read head calibration data'
    character(60) :: MUSG_HeadCalibrationToTecplot_CMD      ='modflow-usg head calibration to tecplot'
    character(60) :: MUSG_RiverConductanceUpdate_CMD        ='river conductance update'
    character(60) :: MUSG_PEST_WellRatePenalties_CMD        ='pest well rate penalties'
    character(60) :: MUSG_PEST_UpdateWellRatePenalties_CMD  ='update well rate penalties'
    character(60) :: MUSG_PEST_FlowSourceCapture_CMD        ='flowsource capture'
    !character(60) :: MUSG_PEST_CLNFileCalculations_CMD      ='CLN file calculations'
    character(60) :: MUSG_PEST_EIWellCLNFileUpdate_CMD      ='ei well CLN file update'
    character(60) :: MUSG_PEST_CountParticlesToWells_CMD    ='count particles to wells'
    character(60) :: MUSG_PEST_ExternalCodeExecute_CMD      ='external code execute'
    character(60) :: MUSG_PEST_RTWellOptimization_CMD       ='rt well optimization'

    ! legacy Modflow 2005 and Modpath 5 version for Nicole (requested by Argha Namhata, Perth)
    character(60) :: M2005_PEST_CountParticlesToWells_CMD='count particles to wells modflow2005 modpath5'
! -------------------------------------------------------------------------------------------
!   subroutine MUSG_ReadAsciiHeadFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        integer :: i1
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Head file: '//FName)
!
!        allocate(Modflow.GWF.head(Modflow.GWF.nCells,1))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!        read(itmp,*) line
!	    read(itmp,*) (i1, Modflow.GWF.head(j,1),j=1,Modflow.GWF.nCells)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiHeadFile
!
!
!
!
!    subroutine MUSG_ReadAsciiKxFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: j,nNodesPerElement
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        real(dr) :: top
!        real(dr) :: bot
!
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Kx file: '//FName)
!
!        allocate(Modflow.Kx(Modflow.GWF.nCells))
!        allocate(Modflow.Thick(Modflow.GWF.nCells))
!        allocate(Modflow.T(Modflow.GWF.nCells))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Kx(j),j=1,Modflow.GWF.nCells)
!	    call freeunit(FNum)
!
!        do j=1,Modflow.GWF.nCells
!            top=0.0
!            bot=0.0
!            do nNodesPerElement=1,4
!                top=top+Modflow.GWF.z(Modflow.GWF.iNode(nNodesPerElement,j))/4.0d0
!            end do
!            do nNodesPerElement=5,8
!                bot=bot+Modflow.GWF.z(Modflow.GWF.iNode(nNodesPerElement,j))/4.0d0
!            end do
!
!            if(j==3657) then
!                continue
!            end if
!
!            if (abs(Modflow.GWF.head(j,1)-999.0d0) < 1e-5 ) then                           ! inactive
!                Modflow.Thick(j)=0.0
!                continue
!            else if (Modflow.GWF.head(j,1) < bot) then                           ! dry
!                Modflow.Thick(j)=0.0
!                continue
!            else if(Modflow.GWF.head(j,1) < top .and. Modflow.GWF.head(j,1) > bot) then    ! partially saturated
!                Modflow.Thick(j)=Modflow.GWF.head(j,1)-bot
!                continue
!            else
!                Modflow.Thick(j)=top-bot                              ! saturated
!                continue
!            endif
!
!            Modflow.T(j)=Modflow.Thick(j)*Modflow.Kx(j)
!
!        end do
!
!
!        continue
!
!    end subroutine MUSG_ReadAsciiKxFile
!   subroutine MUSG_ReadAsciiSsFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Ss file: '//FName)
!
!        allocate(Modflow.Ss(Modflow.GWF.nCells))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.ss(j),j=1,Modflow.GWF.nCells)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiSsFile
!   subroutine MUSG_ReadAsciiSyFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Sy file: '//FName)
!
!        allocate(Modflow.Sy(Modflow.GWF.nCells))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Sy(j),j=1,Modflow.GWF.nCells)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiSyFile
!   subroutine MUSG_ReadAsciiVanisFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Vanis file: '//FName)
!
!        allocate(Modflow.Vanis(Modflow.GWF.nCells))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Vanis(j),j=1,Modflow.GWF.nCells)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiVanisFile
!   subroutine MUSG_ReadRiverFlowsAsciiFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: Line
!        real(dr) :: r1
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'River Flows file: '//FName)
!
!        read(FNum,'(a)') line  ! header
!        do
!            read(FNum,*,iostat=status) r1
!	        if(status /= 0) exit
!
!            Modflow.nlines=Modflow.nlines+1
!        end do
!        Modflow.nlines=Modflow.nlines-1
!        allocate(Modflow.StressPeriod(Modflow.nlines), &
!                    & Modflow.RiverCell(Modflow.nlines), &
!                    & Modflow.RiverFlow(Modflow.nlines), &
!                    & Modflow.RiverHead(Modflow.nlines), &
!                    & Modflow.RiverElev(Modflow.nlines), &
!                    & Modflow.RiverCond(Modflow.nlines), &
!                    & Modflow.RiverConc(Modflow.nlines))
!
!        rewind(FNum)
!        read(FNum,'(a)') line  ! header
!        read(FNum,*) (Modflow.StressPeriod(i), r1, &
!                        & Modflow.RiverCell(i), &
!                        & Modflow.RiverFlow(i), &
!                        & Modflow.RiverHead(i), &
!                        & Modflow.RiverElev(i), &
!                        & Modflow.RiverCond(i), &
!                        & Modflow.RiverConc(i), &
!        i=1,Modflow.nlines)
!
!
!        continue
!
!    end subroutine MUSG_ReadRiverFlowsAsciiFile
!   subroutine MUSG_RiverFlowsToTecplot(FNumMUT,Modflow) !--- Dump river flow data by cell to tecplot .dat file
!        implicit none
!
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (ModflowProject) Modflow
!
!        integer :: LastStressPeriod
!        character(MAXSTRING) :: VarBuffer
!        character(MAXSTRING) :: OutputBuffer
!        integer :: i, j
!        integer :: iFile
!        integer :: NCellFluxvsT, iCell
!
!        read(FNumMUT,'(a)') FName
!        l1=index(FName,'.dat')
!        if(l1==0) then
!            FNAME=trim(FName)//'.tecplot.dat'
!        endif
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//trim(FName))
!
!        VarBuffer='variables="X","Y","Z","Flux(ft3/d)","Head(ft)","Conductance(ft/d)","Elevation(ft)","Concentration(ug/L)","Elapsed Time (days)","Cell"'
!        write(FNum,'(a)') trim(VarBuffer)
!
!        LastStressPeriod=0
!        OutputBuffer=''
!        ifile=0
!        do i=1,Modflow.nlines
!            if(Modflow.StressPeriod(i) /= LastStressPeriod) then
!                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriod(i),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriod(i))
!                LastStressPeriod=Modflow.StressPeriod(i)
!            endif
!            write(FNum,'(9f20.4,i8)') Modflow.GWF.xCell(Modflow.RiverCell(i)),Modflow.GWF.yCell(Modflow.RiverCell(i)),Modflow.GWF.zCell(Modflow.RiverCell(i)), &
!                                    & Modflow.RiverFlow(i), &
!                                    & Modflow.RiverHead(i), &
!                                    & Modflow.RiverElev(i), &
!                                    & Modflow.RiverCond(i), &
!                                    & Modflow.RiverConc(i), &
!                                    & modflow.TIMOT(Modflow.StressPeriod(i)), &
!                                    & Modflow.RiverCell(i)
!        end do
!
!        call FreeUnit(FNum)
!
!        read(FNumMUT,*,iostat=status,end=10) NCellFluxvsT
!	    if(status == 0 .and. NCellFluxvsT>0 ) then
!            do i=1,NCellFluxvsT
!                read(FNumMUT,*) iCell
!                TmpSTR=FileNumberString(icell)
!                FName='cell_'//trim(TmpSTR)//'.dat'
!                call OpenAscii(FNum,FName)
!                call Msg( 'Cell flux to File: '//trim(FName))
!
!                write(FNum,'(a)') 'variables="Time(days)","Flux(ft3/d)"'
!                write(FNum,'(a,i8,a)') 'zone t= "Cell ',iCell,'"'
!
!                do j=1,Modflow.nlines
!                    if(Modflow.RiverCell(j)==iCell) then
!                        write(FNum,'(9f20.4,i8)') modflow.TIMOT(Modflow.StressPeriod(j)), Modflow.RiverFlow(j)
!                    endif
!                end do
!                call FreeUnit(FNum)
!
!            end do
!        endif
!
!
!10      continue
!
!    end subroutine MUSG_RiverFlowsToTecplot
!   subroutine MUSG_ReadHeadCalibrationAsciiFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: Line
!        real(dr) :: r1
!
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'River Flows file: '//FName)
!
!        read(FNum,'(a)') line  ! header
!        do
!            read(FNum,*,iostat=status) r1
!	        if(status /= 0) exit
!
!            Modflow.nlinesHead=Modflow.nlinesHead+1
!        end do
!
!        allocate(     Modflow.StressPeriodHead(Modflow.nlinesHead), &
!                    & Modflow.WellNameHead(Modflow.nlinesHead), &
!                    & Modflow.Xhead(Modflow.nlinesHead), &
!                    & Modflow.YHead(Modflow.nlinesHead), &
!                    & Modflow.ZminHead(Modflow.nlinesHead), &
!                    & Modflow.ZmaxHead(Modflow.nlinesHead), &
!                    & Modflow.Observed_ft(Modflow.nlinesHead), &
!                    & Modflow.Simulated_ft(Modflow.nlinesHead))
!        rewind(FNum)
!        read(FNum,'(a)') line  ! header
!        read(FNum,*) (   Modflow.StressPeriodHead(i), &
!                       & Modflow.WellNameHead(i), &
!                       & Modflow.Xhead(i), &
!                       & Modflow.YHead(i), &
!                       & Modflow.ZminHead(i), &
!                       & Modflow.ZmaxHead(i), &
!                       & Modflow.Observed_ft(i), &
!                       & Modflow.Simulated_ft(i), &
!        & i=1,Modflow.nlinesHead)
!
!
!        continue
!
!    end subroutine MUSG_ReadHeadCalibrationAsciiFile
!   subroutine MUSG_HeadCalibrationToTecplot(FNumMUT,Modflow) !--- Dump Head Calibration data to tecplot .dat file
!        implicit none
!
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (ModflowProject) Modflow
!
!        character(MAXSTRING) :: VarBuffer
!        character(MAXSTRING) :: OutputBuffer
!        integer :: i
!        integer :: iFile
!
!        real(dr) :: zmidpoint
!        real(dr) :: obs
!        real(dr) :: sim
!        integer :: navg
!
!        real(dr) :: TZeroDateNum
!
!        read(FNumMUT,'(a)') FName
!        l1=index(FName,'.dat')
!        if(l1==0) then
!            FNAME=trim(FName)//'.tecplot.dat'
!        endif
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//trim(FName))
!        VarBuffer='variables="X","Y","Z","Observed(ft)","Simulated(ft)","Residual(ft)","Stress Period","DateNum"'
!        write(FNum,'(a)') trim(VarBuffer)
!
!        read(FNumMUT,*) TZeroDateNum
!
!
!        OutputBuffer=''
!        ifile=0
!
!         initialize with first stress period and first well data (i.e. first record)
!        write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriodHead(1),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriodHead(1))
!        navg=1
!        obs=Modflow.Observed_ft(1)
!        sim=Modflow.Simulated_ft(1)
!        do i=2,Modflow.nlinesHead
!            if(Modflow.StressPeriodHead(i) == 208) then
!                continue
!            endif
!            if(Modflow.StressPeriodHead(i) /= Modflow.StressPeriodHead(i-1)) then  ! end of stress period and well data
!                 Calculate average and write last reading
!                zmidpoint=(Modflow.ZminHead(i-1)+Modflow.ZmaxHead(i-1))/2.0
!                write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(i-1),Modflow.yhead(i-1),zmidpoint, &
!                    & obs/navg, &
!                    & sim/navg, &
!                    & (sim-obs)/navg, &
!                    & Modflow.StressPeriodHead(i-1), &
!                    & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!                write(FNum,'(a)')  '% '//Modflow.WellNameHead(i-1)
!
!
!                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriodHead(i),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriodHead(i))
!                navg=1
!                obs=Modflow.Observed_ft(i)
!                sim=Modflow.Simulated_ft(i)
!            else  ! stress period reading i
!                if(Modflow.WellNameHead(i) == Modflow.WellNameHead(i-1)) then ! same well as last reading, update avg
!                    navg=navg+1
!                    obs=obs+Modflow.Observed_ft(i)
!                    sim=sim+Modflow.Simulated_ft(i)
!                else  ! different well than last reading
!                     Calculate average and write last reading
!                    zmidpoint=(Modflow.ZminHead(i-1)+Modflow.ZmaxHead(i-1))/2.0
!                    write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(i-1),Modflow.yhead(i-1),zmidpoint, &
!                           & obs/navg, &
!                           & sim/navg, &
!                           & (sim-obs)/navg , &
!                           & Modflow.StressPeriodHead(i-1), &
!                           & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!                    write(FNum,'(a)')  '% '//Modflow.WellNameHead(i-1)
!                     initialize with this reading
!                    navg=1
!                    obs=Modflow.Observed_ft(i)
!                    sim=Modflow.Simulated_ft(i)
!
!                 endif
!
!            endif
!        end do
!         Calculate average and write last reading
!        zmidpoint=(Modflow.ZminHead(Modflow.nlinesHead)+Modflow.ZmaxHead(Modflow.nlinesHead))/2.0
!        write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(Modflow.nlinesHead),Modflow.yhead(Modflow.nlinesHead),zmidpoint, &
!            & obs/navg, &
!            & sim/navg, &
!            & (sim-obs)/navg, &
!            & Modflow.StressPeriodHead(i-1), &
!            & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!        write(FNum,'(a)')  '% '//Modflow.WellNameHead(Modflow.nlinesHead)
!
!
!        call FreeUnit(FNum)
!
!    end subroutine MUSG_HeadCalibrationToTecplot
!    subroutine MUSG_ReadWellConstructionCSVFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Well construction details file: '//FName)
!
!         Count well data
!        Modflow.nWellConst=0
!        read(FNum,*) line ! throw away header
!        do
!            read(FNum,'(a)',iostat=status) line
!            if(status/=0) then
!                exit
!            end if
!
!            Modflow.nWellConst=Modflow.nWellConst+1
!        end do
!
!        allocate(Modflow.NameWellConst(Modflow.nWellConst), &
!	        & Modflow.XWellConst(Modflow.nWellConst), &
!	        & Modflow.YWellConst(Modflow.nWellConst), &
!	        & Modflow.BotElevWellConst(Modflow.nWellConst), &
!	        & Modflow.TopElevWellConst(Modflow.nWellConst), &
!	        & Modflow.CasingRadiusWellConst(Modflow.nWellConst), &
!	        & Modflow.TonWellConst(Modflow.nWellConst), &
!	        & Modflow.ToffWellConst(Modflow.nWellConst))
!
!        rewind(FNum)
!
!        read(FNum,*) line
!        do i=1,Modflow.nWellConst
!	        read(FNum,*) Modflow.NameWellConst(i), &
!	            & Modflow.XWellConst(i), &
!	            & Modflow.YWellConst(i), &
!	            & Modflow.BotElevWellConst(i), &
!	            & Modflow.TopElevWellConst(i), &
!	            & Modflow.CasingRadiusWellConst(i), &
!	            & Modflow.TonWellConst(i), &
!	            & Modflow.ToffWellConst(i)
!        end do
!
!        continue
!
!	    call freeunit(FNum)
!
!        continue
!
!   end subroutine MUSG_ReadWellConstructionCSVFile
!    subroutine MUSG_Read_EIWellCSVFile(FNumMUT,Modflow)
!        implicit none
!
!        integer :: i, i1, i2
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        type (ModflowProject) Modflow
!
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'EI well construction details file: '//FName)
!
!         Count well data
!        Modflow.n_EIWell=0
!        read(FNum,*) line ! throw away header
!        do
!            read(FNum,'(a)',iostat=status) line
!            if(status/=0) then
!                exit
!            end if
!
!            Modflow.n_EIWell=Modflow.n_EIWell+1
!        end do
!
!        allocate(Modflow.Name_EIWell(Modflow.n_EIWell), &
!	        & Modflow.X_EIWell(Modflow.n_EIWell), &
!	        & Modflow.Y_EIWell(Modflow.n_EIWell), &
!	        & Modflow.TopElev_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenALength_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenBOffset_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenBLength_EIWell(Modflow.n_EIWell))
!
!        rewind(FNum)
!
!        read(FNum,'(a)') line
!        do i=1,Modflow.n_EIWell
!	        read(FNum,*) Modflow.Name_EIWell(i), &
!	            & Modflow.X_EIWell(i), &
!	            & Modflow.Y_EIWell(i), &
!	            & Modflow.TopElev_EIWell(i), &
!	            & Modflow.ScreenALength_EIWell(i), &
!	            & Modflow.ScreenBOffset_EIWell(i), &
!	            & Modflow.ScreenBLength_EIWell(i)
!            i1=index(Modflow.Name_EIWell(i),'IW')
!            i2=index(Modflow.Name_EIWell(i),'EW')
!            if(i1==0 .and. i2==0) then
!                call ErrMsg('Extraction/Injection well name "'//trim(Modflow.Name_EIWell(i))//'" must contain the string "IW" (injection well) or "EW" (extraction well)')
!            endif
!        end do
!
!        continue
!
!	    call freeunit(FNum)
!
!        continue
!
!   end subroutine MUSG_Read_EIWellCSVFile
!
!
!   subroutine MUSG_RiverConductanceUpdate(FNumMUT) !--- Dump Head Calibration data to tecplot .dat file
!        implicit none
!
!        integer :: FNumMUT
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumOut
!        character(MAXLBL) :: FNameOut
!
!
!        integer :: i
!
!        integer :: nRiv
!        real(dr) :: length_ft(1000)
!        integer :: node(1000)
!        integer :: zonenum(1000)
!        character*80 :: line
!        real(dr) :: Cond_d_Len(1000)
!
!        real(dr) :: a2, a4, a5
!        integer :: i1, i2
!        real(dr) :: oldCond, newCond
!
!
!         file 1
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 1: '//trim(FName))
!
!        i=1
!        read(FNum,'(a)') line
!        do
!           read(FNum,*,iostat=status) node(i),Length_ft(i),ZoneNum(i)
!           if(status /= 0) exit
!
!           i=i+1
!        end do
!        nRiv=i-1
!        call FreeUnit(FNum)
!
!
!         file 2
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 2: '//trim(FName))
!
!        read(FNum,'(a)') line
!        do
!           read(FNum,*,iostat=status)  i, Cond_d_Len(i)
!           if(status /= 0) exit
!
!        end do
!
!         file 3
!        read(FNumMUT,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 3: '//trim(FName))
!
!         file Out
!        read(FNumMUT,'(a)') FNameOut
!        call OpenAscii(FNumOut,FNameOut)
!        call Msg( 'File Out: '//trim(FNameOut))
!
!
!        read(FNum,'(a)') line
!        write(FNumOut,'(a)') line
!        read(FNum,'(a)') line
!        write(FNumOut,'(a)') line
!        do
!            read(FNum,'(a)',iostat=status) line
!            if(status /= 0) exit
!
!            write(FNumOut,'(a)') line
!            do i=1,nRiv
!                read(FNum,*) i1, a2, oldCond, a4, a5, i2
!                newCond=Length_ft(i)*Cond_d_Len(i)
!                if(abs(newcond-oldcond) > 1e-3) then
!                    continue
!                endif
!                write(FNumOut,'(i8,1x,f12.6,1x,1pe15.6,1x,f12.6,1x,1pe15.6,i5)') i1, a2, newCond, a4, a5, i2
!            end do
!        end do
!        call FreeUnit(FNum)
!        call FreeUnit(FNumOut)
!        continue
!
!    end subroutine MUSG_RiverConductanceUpdate
!    subroutine MUSG_PEST_WellRatePenalties(FNumMUT) !--- Overwrite rates in Modflow-USG well file
!        implicit none
!
!        integer :: i, j
!
!        integer :: FNumMUT
!        integer :: FnumPenalty
!        character(MAXLBL) :: FNamePenalty
!        integer :: nRange
!        real(dr), allocatable :: MinPenalty(:)
!        real(dr), allocatable :: MaxPenalty(:)
!        real(dr), allocatable :: MinRange(:)
!        real(dr), allocatable :: MaxRange(:)
!        real(dr) :: InjRatePercentTargetLowerMin
!        real(dr) :: InjRatePercentTargetLowerMax
!        real(dr) :: InjratePercentPenaltyLowerMin
!        real(dr) :: InjratePercentPenaltyLowerMax
!        real(dr) :: InjRatePercentTargetHigher
!        real(dr) :: InjratePercentPenaltyHigher
!        real(dr) :: PESTStressPeriodInjWellFlips
!        real(dr) :: PESTStressPeriodExtWellFlips
!
!
!        real(dr) :: MinExtRate
!        real(dr) :: MaxInjRateGPM
!
!        integer :: FnumExtWell
!        character(MAXLBL) :: FNameExtWell
!        integer :: nExtWell
!        character(20), allocatable :: ExtWellName(:)
!        real(dr), allocatable :: ExtWellRate(:,:)
!
!        integer :: FnumInjWell
!        character(MAXLBL) :: FNameInjWell
!        integer :: nInjWell
!        character(20), allocatable :: InjWellName(:)
!        real(dr), allocatable :: InjWellFraction(:,:)
!
!        integer :: nStressPeriods
!
!         CLN file
!        integer :: FnumCLN
!        character(MAXLBL) :: FNameCLN
!        integer :: nCLN
!        integer :: iCLNFirstEWIW
!        integer, allocatable :: CLNType(:)   ! 0 - conventional well, 1 - extaction (EW) well, 2 - injection (IW) well
!        integer, parameter :: EW_upper=11
!        integer, parameter :: EW_lower=12
!        integer, parameter :: IW_upper=21
!        integer, parameter :: IW_lower=22
!
!         Calculations
!        real(dr) :: sumExtWellRates
!        real(dr) :: sumInjWellFractions
!        real(dr), allocatable :: InjWellRate(:,:)
!
!         Modflow-USG well file
!        integer :: FnumMUSGWell
!        character(MAXLBL) :: FNameMUSGWell
!        integer :: FnumMUSGWellOut
!        character(MAXLBL) :: FNameMUSGWellOut
!        integer :: iStressPeriod
!        character(MAXLBL) :: line
!        character(20) :: ThisWellName
!        logical :: WellFound
!        integer :: CLN
!        real(dr) :: rate
!
!         Modflow-USG transient ibound file
!        integer :: FnumTIB
!        character(MAXLBL) :: FNameTIB
!        integer :: FnumTIBOut
!        character(MAXLBL) :: FNameTIBOut
!        integer :: nCLNTurnOff
!        integer :: nCLNTurnOn
!        integer :: nCells
!        integer :: iDum2
!        integer :: nEWIWTurnOff
!        integer :: nEWIWTurnOn
!        integer :: nlist
!        integer :: CLNArray(MAXCLN)
!
!
!
!         Penalties input file
!        read(FNumMUT,'(a)') FNamePenalty
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties file: '//trim(FNamePenalty))
!
!
!        read(FNumPenalty,*) MinExtRate
!        write(TMPStr,'(4f15.3)') MinExtRate
!        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
!        read(FNumPenalty,*) nRange
!        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
!        do i=1,nRange
!            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
!            if(i>1) then
!                if(Minrange(i)/=MaxRange(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min range 2 not equal to max range 1')
!                end if
!                if(MinPenalty(i)/=MaxPenalty(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
!                end if
!            end if
!        end do
!
!        read(FNumPenalty,*) MaxInjRateGPM
!        write(TMPStr,*) MaxInjRateGPM
!        call Msg( 'Maximum injection rate: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
!        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
!        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
!
!
!
!         Extraction well file
!        read(FNumPenalty,'(a)') FNameExtWell
!        call OpenAscii(FNumExtWell,FNameExtWell)
!        call Msg( 'Extraction well file: '//trim(FNameExtWell))
!
!        read(FNumExtWell,*) nExtWell
!        read(FNumExtWell,*) nStressPeriods
!        allocate(ExtWellName(nExtWell),ExtWellRate(nExtWell,nStressPeriods))
!        do i=1,nExtWell
!            read(FNumExtWell,*) ExtWellName(i)
!            do j=1,nStressPeriods
!                read(FNumExtWell,*) ExtWellRate(i,j)
!            end do
!        end do
!
!        call FreeUnit(FNumExtWell)
!
!         Injection well file
!        read(FNumPenalty,'(a)') FNameInjWell
!        call OpenAscii(FNumInjWell,FNameInjWell)
!        call Msg( 'Injection well file: '//trim(FNameInjWell))
!
!        read(FNumInjWell,*) nInjWell
!        read(FNumExtWell,*) nStressPeriods
!        allocate(InjWellName(nInjWell),InjWellFraction(nInjWell,nStressPeriods),InjWellRate(nInjWell,nStressPeriods))
!
!        do i=1,nInjWell
!            read(FNumInjWell,*) InjWellName(i)
!            do j=1,nStressPeriods
!                read(FNumExtWell,*)InjWellFraction(i,j)
!            end do
!        end do
!        call FreeUnit(FNumInjWell)
!
!        call FreeUnit(FNumPenalty)
!
!         CLN information file
!        read(FNumMUT,'(a)') FNameCLN
!        call OpenAscii(FNumCLN,FNameCLN)
!        call Msg( 'CLN file: '//trim(FNameCLN))
!
!        read(FNumCLN,*) nCLN
!
!        allocate(CLNType(nCLN))
!        CLNType(:)=0  ! by default a conventional well
!
!        read(FNumCLN,'(a)') line   ! throw away line 2
!         determine CLN mumber of first extraction/injection well and flag CLNType
!        iCLNFirstEWIW=nCLN+1
!        do i=1,nCLN
!            read(FNumCLN,'(a)') line
!            l1=index(line,'Well = ')
!            if(l1>0) then
!                read(line(l1+7:),'(a)') ThisWellName
!                if(ThisWellName(1:2)=="EW") then
!                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
!                    if(index(ThisWellName,'_A')>0) CLNType(i)=EW_upper
!                    if(index(ThisWellName,'_B')>0) CLNType(i)=EW_lower
!                else if(ThisWellName(1:2)=="IW") then
!                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
!                    if(index(ThisWellName,'_A')>0) CLNType(i)=IW_upper
!                    if(index(ThisWellName,'_B')>0) CLNType(i)=IW_lower
!                endif
!            endif
!        end do
!
!        continue
!
!        do j=1,nStressPeriods
!             Sum extraction well rates
!            sumExtWellRates=0.0d0
!            do i=1,nExtWell
!                sumExtWellRates=sumExtWellrates+abs(ExtWellRate(i,j))
!            end do
!
!             Normalize injection well fractions
!            sumInjWellFractions=0.0d0
!            do i=1,nInjWell
!                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
!            end do
!            do i=1,nInjWell
!                InjWellFraction(i,j)=InjWellFraction(i,j)*1.0d0/sumInjWellFractions
!            end do
!
!             check, new fractions should sum to 1
!            sumInjWellFractions=0.0d0
!            do i=1,nInjWell
!                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
!            end do
!
!            write(TMPStr,'(g15.3)') sumInjWellFractions
!            call Msg('Check sum of normalized injection well fractions is 1: '//TMPStr)
!
!             Calculate injection well rates
!            do i=1,nInjWell
!                InjWellRate(i,j)=InjWellFraction(i,j)*sumExtWellrates
!                if(InjWellRate(i,j)>MaxInjRateGPM) InjWellRate(i,j)=MaxInjRateGPM
!            end do
!
!        end do
!
!
!        call FreeUnit(FNumPenalty)
!
!         Modflow-USG well file input file
!        read(FNumMUT,'(a)') FNameMUSGWell
!        call OpenAscii(FnumMUSGWell,FNameMUSGWell)
!        call Msg( 'Modflow-USG well input file: '//trim(FNameMUSGWell))
!
!         Open new well output file
!        FNameMUSGWellOut='out_'//FNameMUSGWell
!        call OpenAscii(FnumMUSGWellOut,FNameMUSGWellOut)
!        call Msg( 'Modflow-USG well output file: '//trim(FNameMUSGWellOut))
!
!        read(FNumMUSGWell,'(a)') line
!        write(FNumMUSGWellout,'(a)') line
!
!        iStressPeriod=0
!        read_well_file: do
!            read(FNumMUSGWell,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_well_file
!            endif
!
!            l1=index(line,'stress period')
!            if(l1 > 0) then ! new stress period
!                iStressPeriod=iStressPeriod+1
!                 write stress period header line
!                write(FNumMUSGWellout,'(a)') line
!            else  ! this is a well line
!                 Get well name
!                l1=index(line,'well = ')
!                if(l1>0) then
!                    read(line(l1+7:),'(a)') ThisWellName
!                    WellFound=.false.
!
!                     check if is an extraction well
!                    do i=1,nExtWell
!                        if(index(ExtWellName(i),ThisWellName) > 0) then ! found an extraction well
!                            read(line,'(i11,g15.7,a80)') CLN, rate, line
!                            if(iStressPeriod >= nint(PESTStressPeriodExtWellFlips)) then
!                                CLN=CLN+1
!                                line=trim(line)//'_B'    ! append suffix for lower screens
!                            endif
!                            write(FNumMUSGWellout,'(i11,g15.7,a80)') CLN, ExtWellRate(i,iStressPeriod)*192.5d0, line
!                            WellFound=.true.
!                            exit
!                        endif
!                    end do
!
!                     check if is an injection well
!                    if(.not. WellFound) then
!                        do i=1,nInjWell
!                            if(index(InjWellName(i),ThisWellName) > 0) then ! found an injection well
!                                read(line,'(i11,g15.7,a80)') CLN, rate, line
!                                if(iStressPeriod >= nint(PESTStressPeriodInjWellFlips)) then
!                                    CLN=CLN+1
!                                    line=trim(line)//'_B'    ! append suffix for lower screens
!                                endif
!                                write(FNumMUSGWellout,'(i11,g15.7,a80)') CLN, InjWellRate(i,iStressPeriod)*192.5d0, line
!                                WellFound=.true.
!                                exit
!                            endif
!                        end do
!                    endif
!
!                     normal well
!                    if(.not. WellFound) then
!                        write(FNumMUSGWellout,'(a)') line
!                    endif
!                end if
!            endif
!        end do read_well_file
!
!         Modflow-USG transient ibound input file
!        read(FNumMUT,'(a)') FNameTIB
!        call OpenAscii(FnumTIB,FNameTIB)
!        call Msg( 'Modflow-USG transient ibound input file: '//trim(FNameTIB))
!
!        read(FNumMUT,*) nCells
!
!
!         Open new transient ibound output file
!        FNameTIBOut='out_'//FNameTIB
!        call OpenAscii(FnumTIBOut,FNameTIBOut)
!        call Msg( 'Modflow-USG transient ibound output file: '//trim(FNameTIBOut))
!
!        read_tib_file: do
!            read(FnumTIB,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_tib_file
!            endif
!
!
!            l1=index(line,'stress period ')
!            if(l1 > 0) then ! new stress period
!
!                l1=l1+14  ! position at end of string 'stress period '
!
!                TMPStr=line(31:)
!
!                 Extract stress period from line
!                l2=l1+index(line(l1:),':')-2
!                read(line(l1:l2),*) iStressPeriod
!
!                read(line,*) nCLNTurnOff, nCLNTurnOn, idum2
!
!                if(iStressPeriod==1) then ! Always turn off lower screens of new CLN's unless PESTStressPeriodInjWellFlips=1 and/or PESTStressPeriodExtWellFlips=1
!
!                     determine how many more CLN's are going to be turned off
!                    nEWIWTurnoff=nCLNTurnOff
!                    do i=iCLNFirstEWIW,ncln
!                        if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) nEWIWTurnoff=nEWIWTurnoff+1
!                        if(PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) nEWIWTurnoff=nEWIWTurnoff+1
!                    end do
!
!                    write(FNumTIBOut,'(3i10,a)')  nEWIWTurnoff, nCLNTurnOn, idum2, trim(TMPStr)
!
!                    if(nCLNTurnOff>0) then  ! there were convetional well to be turned off
!                        read(FnumTIB,'(a)') line
!                        write(FNumTIBOut,'(a)') line ! always write next line as is
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    else if(nEWIWTurnoff>nCLNTurnOff) then   ! some or all IW/EW screens are turned off
!                        write(FNumTIBOut,'(a)') 'INTERNAL  1  (FREE)  -1  IB0 array'
!
!                         make a list of CLN numbers to be turned off
!                        nList=0
!                        CLNArray(:)=0
!                        do i=iCLNFirstEWIW,ncln
!                            if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) then
!                                nList=nList+1
!                                CLNArray(nlist)=i+ncells
!                            else if (PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) then
!                                nList=nList+1
!                                CLNArray(nlist)=i+ncells
!                            endif
!                        end do
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nlist)
!                    end if
!
!                    if(nCLNTurnOn>0) then
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is
!                        end do
!                    endif
!
!                else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips) .and. &   ! turn on new CLN lower screens for all injection (IW) wells
!                        iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then     ! turn on new CLN lower screens for all extraction (EW) wells
!                     determine how many new CLN's are going to be turned on
!
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!
!                    if(nCLNTurnOff>0) then
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells
!                        else if(CLNType(i)==EW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!
!               else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips)) then ! turn on new CLN lower screens for injection (IW) wells
!                     determine how many new CLN's are going to be turned on
!
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!
!                    if(nCLNTurnOff>0) then
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!
!                else if(iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then ! turn on new CLN lower screens for extraction (EW) wells
!                     determine how many new CLN's are going to be turned on
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!
!                    if(nCLNTurnOff>0) then
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==EW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!
!
!                else
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nCLNTurnOn, idum2, trim(TMPStr)
!                    if(nCLNTurnOff>0) then
!                        read(FnumTIB,'(a)') line
!                        write(FNumTIBOut,'(a)') line ! always write next line as is
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then
!                        do i=1,nCLNTurnOn  ! read and write the next iDum2 lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is
!                        end do
!                    endif
!                    if(iDum2>0) then
!                        continue
!                    endif
!                endif
!
!
!            end if
!
!
!        end do read_tib_file
!
!        continue
!
!
!    end subroutine MUSG_PEST_WellRatePenalties
!
!
!
!    subroutine MUSG_PEST_UpdateWellRatePenalties(FNumMUT) !--- Update rates in Modflow-USG well file
!        implicit none
!
!        integer :: i, j
!
!        integer :: FNumMUT
!        integer :: FnumPenalty
!        character(MAXLBL) :: FNamePenalty
!        integer :: nRange
!        real(dr), allocatable :: MinPenalty(:)
!        real(dr), allocatable :: MaxPenalty(:)
!        real(dr), allocatable :: MinRange(:)
!        real(dr), allocatable :: MaxRange(:)
!        real(dr) :: InjRatePercentTargetLowerMin
!        real(dr) :: InjRatePercentTargetLowerMax
!        real(dr) :: InjratePercentPenaltyLowerMin
!        real(dr) :: InjratePercentPenaltyLowerMax
!        real(dr) :: InjRatePercentTargetHigher
!        real(dr) :: InjratePercentPenaltyHigher
!        real(dr) :: PESTStressPeriodInjWellFlips
!        real(dr) :: PESTStressPeriodExtWellFlips
!
!
!        real(dr) :: MinExtRate
!        real(dr) :: MaxInjRateGPM
!
!
!         CLN file
!        integer :: FnumCLN
!        character(MAXLBL) :: FNameCLN
!        integer :: nCLN
!
!        integer :: FNumWellCompOut
!        character(MAXLBL) :: FNameWellCompOut
!
!         Calculations
!        real(dr) :: sumExtWellRates
!        real(dr) :: ExtWellRatePenalty
!        real(dr) :: sumInjWellRates
!        real(dr) :: InjWellRatePenalty
!
!         Modflow-USG well file
!        integer :: iStressPeriod
!        character(MAXLBL) :: line
!        character(20), allocatable :: ThisWellName(:)
!
!
!        real(dr) :: req_rate
!        real(dr) :: act_rate
!        integer :: icln
!
!        real(dr) :: RateRatio
!
!
!         CLN information file
!        read(FNumMUT,'(a)') FNameCLN
!        call OpenAscii(FNumCLN,FNameCLN)
!        call Msg( 'CLN file: '//trim(FNameCLN))
!
!        read(FNumCLN,*) nCLN
!        allocate(ThisWellName(nCLN))
!        read(FNumCLN,'(a)') line   ! throw away line 2
!         store well names
!        do i=1,nCLN
!            read(FNumCLN,'(a)') line
!            l1=index(line,'Well = ')
!            if(l1>0) then
!                read(line(l1+7:),'(a)') ThisWellName(i)
!            endif
!        end do
!
!         Penalties input file
!        read(FNumMUT,'(a)') FNamePenalty
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties file: '//trim(FNamePenalty))
!
!
!        read(FNumPenalty,*) MinExtRate
!        write(TMPStr,'(4f15.3)') MinExtRate
!        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
!        read(FNumPenalty,*) nRange
!        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
!        do i=1,nRange
!            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
!            if(i>1) then
!                if(Minrange(i)/=MaxRange(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min range 2 not equal to max range 1')
!                end if
!                if(MinPenalty(i)/=MaxPenalty(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
!                end if
!            end if
!        end do
!
!        read(FNumPenalty,*) MaxInjRateGPM
!        write(TMPStr,*) MaxInjRateGPM
!        call Msg( 'Maximum injection rate: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
!        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
!        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
!
!
!         Wellcomp output  file
!        read(FNumMUT,'(a)') FNameWellCompOut
!        call OpenAscii(FNumWellCompOut,FNameWellCompOut)
!        call Msg( 'WellComp output file: '//trim(FNameWellCompOut))
!
!         Penalties output file
!        FNamePenalty='PenaltiesBySP.txt'
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties by stress period output file: '//trim(FNamePenalty))
!        write(FNumPenalty,'(a)') '      SP#          ExtRate             ExtPenalty            InjRate           InjPenalty'
!
!        read_wellcomp_file: do
!            read(FNumWellCompOut,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_wellcomp_file
!            endif
!
!            l1=index(line,'Stress period = ')
!            if(l1 > 0) then ! new stress period
!                read(line(l1+16:),*) iStressPeriod
!                sumExtWellRates=0.0d0
!                sumInjWellRates=0.0d0
!                stress_period: do
!                    read(FNumWellCompOut,'(a)') line  ! blank line
!                    l1=index(line,'Node         Connection')
!                    if(l1 > 0) then ! Start reading CLN infoes
!                        cln_lines: do
!                            read(FNumWellCompOut,'(a)') line
!                            l1=index(line,'CLN')
!                            if(l1 > 0) then ! read and process CLN info
!                                read(line,*) icln
!                                read(line(l1+4:),*) req_rate,act_rate
!                                if(index(ThisWellName(icln),'EW') > 0) then  ! extraction well
!                                    sumExtWellRates=sumExtWellrates+abs(act_rate)/192.5d0
!                                else if(index(ThisWellName(icln),'IW') > 0) then ! injection well
!                                    sumInjWellRates=sumInjWellRates+abs(act_rate)/192.5d0
!                                end if
!                            else
!                                do j=1,nrange
!                                    if(sumExtWellrates>= MinRange(j) .and. sumExtWellRates<= MaxRange(j)) then ! linear interpolation for penalty
!                                        ExtWellRatePenalty= MinPenalty(j)+(sumExtWellRates-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
!                                    endif
!                                end do
!
!                                InjWellRatePenalty=0.0d0
!                                if(sumExtWellRates == 0.0d0) then
!                                    RateRatio=1.0d0
!                                else
!                                    RateRatio=sumInjWellRates/sumExtWellRates
!
!                                    if(RateRatio < InjRatePercentTargetLowerMax) then
!
InjWellRatePenalty=InjRatePercentPenaltyLowerMin+(InjRatePercentTargetLowerMax-RateRatio)/(InjRatePercentTargetLowerMax-InjRatePercentTargetLowerMin) &
!                                &        * (InjRatePercentPenaltyLowerMax-InjRatePercentPenaltyLowerMin)
!                                    else if(RateRatio > InjRatePercentTargetHigher) then
!                                        InjWellRatePenalty=InjRatePercentPenaltyHigher
!                                    endif
!
!                                endif
!
!                                write(FNumPenalty,'(i8,4f20.5)') IstressPeriod, sumExtWellRates, ExtWellRatePenalty, RateRatio, InjWellRatePenalty
!
!                                exit stress_period
!                            endif
!                        end do cln_lines
!                    end if
!                end do stress_period
!            end if
!
!        end do read_wellcomp_file
!
!    end subroutine MUSG_PEST_UpdateWellRatePenalties
!
!    subroutine MUSG_PEST_FlowSourceCapture(FNumMUT) !--- Average flows for
!        USE IFPORT
!        implicit none
!
!        integer :: i, j
!
!        LOGICAL(4) result
!
!        integer :: FNumMUT
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!        character(MAXLBL) :: FlowSourceDir
!        character(MAXLBL) :: CMD_line
!        character(MAXLBL) :: line
!        integer :: nCSVFiles
!        integer :: FnumCSVtmp
!        integer :: nCells
!
!        real(dr), allocatable :: Flowthrough(:,:)
!        integer :: iDum
!
!        integer :: iCSV
!        integer :: iGroup
!        integer :: FnumCellList
!        character(MAXLBL), allocatable :: FNameCellList(:)
!        real(dr), allocatable :: FS_Average(:,:)
!        integer :: jCell
!        integer :: nCellListFiles
!        integer, allocatable :: nCountGroup(:)
!        real(dr), allocatable :: StressPeriod(:)
!        integer, allocatable :: indx_col(:)
!
!
!        integer :: FnumGroupAverage
!        character(MAXLBL) :: FNameGroupAverage
!
!        read(FNumMUT,'(a)') FlowSourceDir
!
!         All of the .csv file in the directory FlowSourceDir are flowsource output files
!        CMD_line='dir /b '//trim(FlowSourceDir)//'\*.csv > csvfiles'
!        result = SYSTEMQQ(CMD_line)
!
!         count the number of csv files
!        FNameCSV='csvfiles'
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg('CSV filenames file: '//trim(FNameCSV))
!        nCSVFiles=0
!        do
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status /= 0) then
!                exit
!            endif
!
!            nCSVFiles=nCSVFiles+1
!        end do
!
!         count the number of cells in the last CSV file
!        call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
!        nCells=0
!        do
!            read(FnumCSVTmp,'(a)',iostat=status) line
!            if(status /= 0) then
!                exit
!            endif
!
!            nCells=nCells+1
!
!        end do
!
!        nCells=nCells-1
!
!        call FreeUnit(FnumCSVTmp)
!
!         dimension flowsource arrays
!        allocate(Flowthrough(nCSVFiles,nCells),StressPeriod(nCSVFiles),indx_col(nCSVFiles))
!
!        rewind(FNumCSV)
!        do i=1,nCSVFiles
!            read(FnumCSV,'(a)',iostat=status) line
!
!             Extract stress period from line
!            l1=index(line,'_Time-')+6
!            l2=l1+index(line(l1:),'_')-2
!            read(line(l1:l2),*) StressPeriod(i)
!
!            call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
!            call Msg('CSV file: '//trim(FlowSourceDir)//'\'//trim(line))
!            read(FnumCSVTmp,'(a)') line   ! throw away header
!            do j=1,nCells
!                read(FnumCSVTmp,*) iDum,Flowthrough(i,j)
!            end do
!        end do
!
!         Process cell list files
!        read(FNumMUT,*) nCellListFiles
!        allocate(nCountGroup(nCellListFiles),FS_average(nCSVFiles,nCellListFiles),FNameCellList(nCellListFiles))
!        nCountGroup(:)=0
!        FS_average(:,:)=0.0d0
!        do iGroup=1,nCellListFiles
!            read(FNumMUT,'(a)') FNameCellList(iGroup)
!            call OpenAscii(FnumCellList,FNameCellList(iGroup))
!            call Msg('Cell list file: '//trim(FNameCellList(iGroup)))
!            do
!                read(FnumCellList,*,iostat=status) jCell
!                if(status/=0) then
!                    exit
!                endif
!
!                nCountGroup(iGroup)=nCountGroup(iGroup)+1
!
!                do iCSV=1,nCSVFiles
!                    FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)+Flowthrough(iCSV,jCell)
!                end do
!
!            end do
!
!        end do
!
!         Calculate average for each group and csv file
!        do iGroup=1,nCellListFiles
!            do iCSV=1,nCSVFiles
!                FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)/nCountGroup(iGroup)
!            end do
!        end do
!
!         Sort by StressPeriod. Order written to indx_col.
!        call indexx3(nCSVFiles,StressPeriod,indx_col)
!
!
!
!        continue
!
!         Write one output file per group
!        do iGroup=1,nCellListFiles
!            FNameGroupAverage='avg_'//trim(FNameCellList(iGroup))//'.csv'
!            call OpenAscii(FNumGroupAverage,FNameGroupAverage)
!            call Msg('Group FS average csv output file: '//trim(FNameGroupAverage))
!            write(FNumGroupAverage,'(a)') "Stress Period, Average Capture"
!            do iCSV=1,nCSVFiles
!               write(FNumGroupAverage,'(f10.0,a,f12.5)') StressPeriod(indx_col(iCSV)),',',FS_average(indx_col(iCSV),iGroup)*100.0d0
!            end do
!            call FreeUnit(FNumGroupAverage)
!        end do
!
!
!
!    end subroutine MUSG_PEST_FlowSourceCapture
!
!    subroutine MUSG_PEST_ExternalCodeExecute(FNumMUT) !--- Execute an external program as defined in the .tg file.
!        USE IFPORT
!        implicit none
!
!        integer :: FNumMUT
!        LOGICAL(4) result
!        character(MAXLBL) :: CMD_line
!
!        read(FNumMUT,'(a)') CMD_line
!        result = SYSTEMQQ(trim(CMD_line))
!
!    end subroutine MUSG_PEST_ExternalCodeExecute

    !subroutine MUSG_PEST_CLNFileCalculations(FNumMUT, Modflow) !--- Given an EI pumping well, calculate the CLN file entries
    !    USE IFPORT
    !    implicit none
    !
    !    type (ModflowProject) Modflow
    !
    !
    !    integer :: i,j, k
    !
    !    !LOGICAL(4) result
    !    !
    !    integer :: FNumMUT
    !
    !    !integer :: FnumCSV
    !    !character(MAXLBL) :: FNameCSV
    !
    !    character(30) :: WellID
    !    real(dr) :: CellHeight
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !
    !    read(FNumMUT,'(a)') WellID
    !    do i=1, Modflow.nWellConst
    !        if(index(Modflow.NameWellConst(i),trim(WellID)) > 0) then
    !
    !            do j=1,Modflow.GWF.nCells/Modflow.GWF.nLayers  ! loop over the cells in layer 1
    !                if(Modflow.XWellConst(i) >= Modflow.GWF.X(Modflow.GWF.iNode(1,j)) .and. Modflow.XWellConst(i) <= Modflow.GWF.X(Modflow.GWF.iNode(4,j))) then
    !
    !                    if(Modflow.YWellConst(i) >= Modflow.GWF.Y(Modflow.GWF.iNode(1,j)) .and. Modflow.YWellConst(i) <= Modflow.GWF.Y(Modflow.GWF.iNode(2,j))) then
    !                        iCellCurr=j
    !                        write(*,'(a,i8,3f15.3)') ' Cell x y z',iCellCurr, Modflow.GWF.xCell(iCellCurr), Modflow.GWF.yCell(iCellCurr), Modflow.GWF.zCell(iCellCurr)
    !                        write(*,*) ' k, vertex(k), Xvertex(k), Yvertex(k), Zvertex(k)'
    !                        do k=1,Modflow.GWF.nNodesPerElement
    !                            write(*,'(i2,i8,3f15.3)') k,
    Modflow.GWF.iNode(k,iCellCurr),Modflow.GWF.X(Modflow.GWF.iNode(k,iCellCurr)),Modflow.GWF.Y(Modflow.GWF.iNode(k,iCellCurr)),Modflow.GWF.Z(Modflow.GWF.iNode(k,iCellCurr))
    !                        end do
    !                        pause
    !                        CurrTopElev=Modflow.TopElevWellConst(i)
    !                        LayerLoop: do k=1,Modflow.GWF.nLayers
    !                            if(CurrTopElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                                if(Modflow.BotElevWellConst(i) > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                    CellHeight=CurrTopElev-Modflow.BotElevWellConst(i)
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    exit LayerLoop
    !                                else
    !                                    CellHeight=CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                    iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                                endif
    !                            else
    !                                iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !
    !                            end if
    !                        end do LayerLoop
    !                    end if
    !                end if
    !            end do
    !
    !           continue
    !        end if
    !
    !    end do
    !
    !end subroutine MUSG_PEST_CLNFileCalculations
    !
    !subroutine MUSG_PEST_EIWellCLNFileUpdate(FNumMUT, Modflow) !--- Given new EI pumping wells, add screens as new CLN file entries
    !    USE IFPORT
    !    implicit none
    !
    !    type (ModflowProject) Modflow
    !
    !
    !    integer :: i,j, k
    !
    !    integer :: FNumMUT
    !
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !    integer :: FnumCLNout
    !    character(MAXLBL) :: FNameCLNout
    !
    !    integer :: FNumFSCtl
    !    character(MAXLBL) :: FnameFSCtl
    !    integer :: FNumFSCtlOut
    !    character(MAXLBL) :: FnameFSCtlOut
    !
    !    integer :: FNumWel
    !    character(MAXLBL) :: FnameWel
    !    integer :: FNumWelOut
    !    character(MAXLBL) :: FnameWelOut
    !
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !    real(dr) :: CurrBotElev
    !    real(dr) :: CurrScreenLength
    !    logical :: ScreenFound
    !    integer :: nEIScreens
    !    real(dr) :: MeshBottom
    !
    !
    !    integer, allocatable :: IFNO(:)
    !    integer, allocatable :: IFTYP(:)
    !    integer, allocatable :: IFDIR(:)
    !    real(dr), allocatable :: FLENG(:)
    !    real(dr), allocatable :: FELEV(:)
    !    real(dr), allocatable :: FANGLE(:)
    !    integer, allocatable :: IFLIN(:)
    !    integer, allocatable :: ICCWADI(:)
    !
    !    integer, allocatable :: nCellList(:)
	   ! character(31), allocatable :: NameEIScreen(:)
    !    integer, allocatable :: CellNumber(:,:)
    !    real(dr), allocatable :: CellScreenLength(:,:)
    !
    !    real(dr), allocatable :: StartingHeads(:)
    !
    !    integer :: i1, i2, i3, i4, i5
    !    integer :: ICLNOrig, ICLNNew, myNCONDUITYP
    !    integer :: ICellListOrig, ICellListNew
    !    integer :: nSum
    !    character(MAXSTRING) :: line
    !
    !    logical :: WellFound
    !
    !    integer :: IWellOrig, IWellNew
    !
    !
    !    allocate(IFNO(2*Modflow.n_EIWell), &
    !    &   IFTYP(2*Modflow.n_EIWell), &
    !    &   IFDIR(2*Modflow.n_EIWell), &
    !    &   FLENG(2*Modflow.n_EIWell), &
    !    &   FELEV(2*Modflow.n_EIWell), &
    !    &   FANGLE(2*Modflow.n_EIWell), &
    !    &   IFLIN(2*Modflow.n_EIWell), &
    !    &   ICCWADI(2*Modflow.n_EIWell))
    !
    !    IFNO(:) = 0
    !    IFTYP(:) = 1
    !    IFDIR(:) = 0
    !    FLENG(:) = 0.0d0
    !    FELEV(:) = 0.0d0
    !    FANGLE(:) = 0.0d0
    !    IFLIN(:) = 1
    !    ICCWADI(:) = 0
    !
    !    allocate(nCellList(2*Modflow.n_EIWell), &
    !    &   NameEIScreen(2*Modflow.n_EIWell), &
    !    &   CellNumber(2*Modflow.n_EIWell,100),&
    !    &   CellScreenLength(2*Modflow.n_EIWell,100))
    !
    !    nEIScreens=0
    !
    !    !-----------------------------------------------------------------------------------
    !    ! Find where screens fit in 3D Modflow domain
    !    WellSearch:do i=1, Modflow.n_EIWell
    !        call Msg('------------------------------------------------------------------------------')
    !        call Msg('EI Well: '//trim(Modflow.Name_EIWell(i)))
    !        write(TmpSTR,'(a, 2f15.3)') 'X Y: ',Modflow.X_EIWell(i), Modflow.Y_EIWell(i)
    !        call Msg(TmpSTR)
    !        CurrBotElev=Modflow.TopElev_EIWell(i)- Modflow.ScreenALength_EIWell(i)
    !        call Msg('       Elevation       Length      Comment')
    !        write(TmpSTR,'(f15.3,a)') Modflow.TopElev_EIWell(i), '           -        Screen A top elevation '
    !        call Msg(TmpSTR)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBOffset_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBLength_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
    !        call Msg(TmpSTR)
    !
    !        WellFound=.false.
    !
    !        do j=1,Modflow.GWF.nCells/Modflow.GWF.nLayers  ! loop over the cells in layer 1
    !            if(Modflow.X_EIWell(i) >= Modflow.GWF.X(Modflow.GWF.iNode(1,j)) .and. Modflow.X_EIWell(i) <= Modflow.GWF.X(Modflow.GWF.iNode(4,j))) then
    !                if(Modflow.Y_EIWell(i) >= Modflow.GWF.Y(Modflow.GWF.iNode(1,j)) .and. Modflow.Y_EIWell(i) <= Modflow.GWF.Y(Modflow.GWF.iNode(2,j))) then
    !                    iCellCurr=j
    !                    WellFound=.true.
    !
    !                    write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', Modflow.GWF.xCell(iCellCurr), Modflow.GWF.yCell(iCellCurr)
    !                    call Msg(TmpSTR)
    !
    !                    write(TmpSTR,'(a,2f15.3)') 'X range ', Modflow.GWF.X(Modflow.GWF.iNode(1,j)), Modflow.GWF.X(Modflow.GWF.iNode(4,j))
    !                    call Msg(TmpSTR)
    !                    write(TmpSTR,'(a,2f15.3)') 'Y range ', Modflow.GWF.Y(Modflow.GWF.iNode(1,j)), Modflow.GWF.Y(Modflow.GWF.iNode(2,j))
    !                    call Msg(TmpSTR)
    !
    !
    !
    !                    call Msg(' Layer  Cell     Vertex         Z      Height')
    !                    write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,Modflow.GWF.Z(Modflow.GWF.iNode(4,iCellCurr))
    !                    call Msg(TmpSTR)
    !                    CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(4,iCellCurr))
    !                    do k=1,Modflow.GWF.nLayers
    !                        write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8,
    Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)),CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        call Msg(TmpSTR)
    !                        if(k==Modflow.GWF.nLayers) then
    !                            MeshBottom= Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        endif
    !                        CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                    end do
    !
    !                    iCellCurr=j
    !
    !
    !                    ! Top screen A
    !                    CurrTopElev=Modflow.TopElev_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenALength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    LayerLoop1: do k=1,Modflow.GWF.nLayers
    !                        if(CurrTopElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr A    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_A'
    !                            end if
    !                            if(CurrBotElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop1
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !
    !                                CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            if(iCellCurr > Modflow.GWF.nCells .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !
    !                        end if
    !                    end do LayerLoop1
    !
    !                    if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !
    !                    ! Screen B
    !                    CurrTopElev=CurrBotElev-Modflow.ScreenBOffset_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenBLength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    iCellCurr=j
    !                    LayerLoop2: do k=1,Modflow.GWF.nLayers
    !                        if(CurrTopElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr B    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_B'
    !                            endif
    !                            if(CurrBotElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop2
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            if(iCellCurr > Modflow.GWF.nCells .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !
    !                        end if
    !                    end do LayerLoop2
    !
    !                     if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !                end if
    !            end if
    !            if(WellFound) cycle WellSearch
    !        end do
    !    end do WellSearch
    !
    !    !-----------------------------------------------------------------------------------
    !    ! Insert wells in Modflow-USG CLN input file
    !    read(FNumMUT,'(a)') FnameCLN
    !    call OpenAscii(FnumCLN,FnameCLN)
    !    call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
    !
    !    ! Open new CLN well output file
    !    FNameCLNOut='out_'//FNameCLN
    !    call OpenAscii(FnumCLNOut,FNameCLNOut)
    !    call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
    !
    !    ! line 1 of CLN file
    !    read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,NCONDUITYP
    !    iCLNNew=iCLNOrig+nEIScreens
    !
    !    allocate(StartingHeads(iCLNNew))
    !
    !    nSum=0
    !    do i=1,nEIScreens
    !        nSum=nSum+nCellList(i)
    !    end do
    !    iCellListNew=iCellListOrig+nSum
    !    write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,myNCONDUITYP
    !
    !    read(FnumCLN,'(a)') line
    !    write(FnumCLNOut,'(a)') line
    !
    !    do i=1,iCLNOrig
    !        read(FnumCLN,'(a)') line
    !        i2=index(line,'Well =')+6
    !        if(index(line(i2:),'IW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"')
    !        endif
    !        if(index(line(i2:),'EW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"')
    !        endif
    !
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNO')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2),
    trim(NameEIScreen(i))
    !    end do
    !
    !    do i=1,iCellListOrig
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNOD')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        do j=1,nCellList(i)
    !            !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !            write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !        end do
    !    end do
    !
    !    do i=1,myNCONDUITYP   ! always myNCONDUITYP lines.  Never less than 1.
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    read(FnumCLN,'(a)') line  ! always an IBOUND line
    !    write(FnumCLNOut,'(a)') line
    !
    !
    !    ! starting heads  iCLNNew=iCLNOrig+nEIScreens
    !    read(FnumCLN,'(a)') line  ! always a starting heads header line
    !    write(FnumCLNOut,'(a)') line
    !    read(FnumCLN,*) (StartingHeads(i),i=1,iCLNOrig)
    !    do i=iCLNOrig+1,iCLNNew
    !        StartingHeads(i)=StartingHeads(iCLNOrig)
    !    end do
    !    write(FnumCLNOut,*) (StartingHeads(i),i=1,iCLNNew)
    !
    !    ! in transport case may be more data so read/write to end of file
    !    do
    !        read(FnumCLN,'(a)',iostat=status) line
    !        if(status/=0) then
    !            exit
    !        endif
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !
    !    !-----------------------------------------------------------------------------------
    !    ! Read well file and insert placeholders for EW/IW wells
    !
    !    ! Modflow Well file
    !    read(FNumMUT,'(a)') FnameWel
    !    call OpenAscii(FNumWel,FnameWel)
    !    call Msg( 'Modflow well file: '//trim(FnameWel))
    !
    !    ! Open new FlowSource control output file
    !    FnameWelOut='place_'//FnameWel
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output file: '//trim(FnameWelOut))
    !
    !    ! Always keep first line
    !    read(FNumWel,'(a)') line
    !    write(FNumWelout,'(a)') line
    !
    !    do
    !        read(FNumWel,'(a)',iostat=status) line
    !        if(status/=0) exit
    !
    !        read(line,*) i1,i2,iWellOrig
    !        iWellNew=iWellOrig+Modflow.n_EIWell
    !
    !        write(FNumWelout,'(3i10,a)') i1,i2,iWellNew,trim(line(31:))
    !        do i=1,IwellOrig
    !            read(FNumWel,'(a)') line
    !            write(FNumWelout,'(a)') line
    !        end do
    !        i1=index(line,'well =')+6
    !        do i=1,Modflow.n_EIWell
    !            write(FNumWelout,'(i11,g14.7,a,a)') iCLNOrig+i*2-1,0.0d0,line(26:i1),trim(Modflow.Name_EIWell(i))
    !        end do
    !
    !
    !    end do
    !    continue
    !
    !
    !    !-----------------------------------------------------------------------------------
    !    ! Flowsource control file
    !    read(FNumMUT,'(a)') FnameFSCtl
    !    call OpenAscii(FNumFSCtl,FnameFSCtl)
    !    call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
    !
    !    ! Open new FlowSource control output file
    !    FnameFSCtlOut='out_'//FnameFSCtl
    !    call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
    !    call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
    !
    !    do
    !        read(FNumFSCtl,'(a)') line
    !        write(FNumFSCtlout,'(a)') line
    !        if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
    !            do i=1,nEIScreens,2
    !                if(index(NameEIScreen(i),'EW')>0) then
    !                    write(FNumFSCtlout,'(a)') ' '
    !                    write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
    !                    do j=1,nCellList(i)
    !                        write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
    !                    end do
    !                    do j=1,nCellList(i+1)
    !                        if(j==1) then
    !                            if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        else
    !                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        endif
    !                    end do
    !                end if
    !            end do
    !            exit
    !        end if
    !    end do
    !
    !    end_ctl: do
    !        read(FNumFSCtl,'(a)') line
    !        if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
    !            write(FNumFSCtlout,'(a)') ' '
    !            write(FNumFSCtlout,'(a)') line
    !
    !            do
    !                read(FNumFSCtl,'(a)',iostat=status) line
    !                if(status /= 0) then
    !                    exit end_ctl
    !                endif
    !
    !                write(FNumFSCtlout,'(a)') line
    !
    !            end do
    !        end if
    !    end do end_ctl
    !
    !
    !end subroutine MUSG_PEST_EIWellCLNFileUpdate
    !subroutine old_MUSG_PEST_EIWellCLNFileUpdate(FNumMUT, Modflow) !--- Given new EI pumping wells, add screens as new CLN file entries
    !    USE IFPORT
    !    implicit none
    !
    !    type (ModflowProject) Modflow
    !
    !
    !    integer :: i,j, k
    !
    !    integer :: FNumMUT
    !
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !
    !    integer :: FnumCLNout
    !    character(MAXLBL) :: FNameCLNout
    !
    !    integer :: FNumFSCtl
    !    character(MAXLBL) :: FnameFSCtl
    !
    !    integer :: FNumFSCtlOut
    !    character(MAXLBL) :: FnameFSCtlOut
    !
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !    real(dr) :: CurrBotElev
    !    real(dr) :: CurrScreenLength
    !    logical :: ScreenFound
    !    integer :: nEIScreens
    !    real(dr) :: MeshBottom
    !
    !
    !    integer, allocatable :: IFNO(:)
    !    integer, allocatable :: IFTYP(:)
    !    integer, allocatable :: IFDIR(:)
    !    real(dr), allocatable :: FLENG(:)
    !    real(dr), allocatable :: FELEV(:)
    !    real(dr), allocatable :: FANGLE(:)
    !    integer, allocatable :: IFLIN(:)
    !    integer, allocatable :: ICCWADI(:)
    !
    !    integer, allocatable :: nCellList(:)
	   ! character(31), allocatable :: NameEIScreen(:)
    !    integer, allocatable :: CellNumber(:,:)
    !    real(dr), allocatable :: CellScreenLength(:,:)
    !
    !    integer :: i1, i2, i3, i4, i5
    !    integer :: ICLNOrig, ICLNNew, myNCONDUITYP
    !    integer :: ICellListOrig, ICellListNew
    !    integer :: nSum
    !    character(MAXSTRING) :: line
    !
    !    logical :: WellFound
    !
    !
    !    allocate(IFNO(2*Modflow.n_EIWell), &
    !    &   IFTYP(2*Modflow.n_EIWell), &
    !    &   IFDIR(2*Modflow.n_EIWell), &
    !    &   FLENG(2*Modflow.n_EIWell), &
    !    &   FELEV(2*Modflow.n_EIWell), &
    !    &   FANGLE(2*Modflow.n_EIWell), &
    !    &   IFLIN(2*Modflow.n_EIWell), &
    !    &   ICCWADI(2*Modflow.n_EIWell))
    !
    !    IFNO(:) = 0
    !    IFTYP(:) = 1
    !    IFDIR(:) = 0
    !    FLENG(:) = 0.0d0
    !    FELEV(:) = 0.0d0
    !    FANGLE(:) = 0.0d0
    !    IFLIN(:) = 1
    !    ICCWADI(:) = 0
    !
    !    allocate(nCellList(2*Modflow.n_EIWell), &
    !    &   NameEIScreen(2*Modflow.n_EIWell), &
    !    &   CellNumber(2*Modflow.n_EIWell,100),&
    !    &   CellScreenLength(2*Modflow.n_EIWell,100))
    !
    !    nEIScreens=0
    !
    !    WellSearch:do i=1, Modflow.n_EIWell
    !        call Msg('------------------------------------------------------------------------------')
    !        call Msg('EI Well: '//trim(Modflow.Name_EIWell(i)))
    !        write(TmpSTR,'(a, 2f15.3)') 'X Y: ',Modflow.X_EIWell(i), Modflow.Y_EIWell(i)
    !        call Msg(TmpSTR)
    !        CurrBotElev=Modflow.TopElev_EIWell(i)- Modflow.ScreenALength_EIWell(i)
    !        call Msg('       Elevation       Length      Comment')
    !        write(TmpSTR,'(f15.3,a)') Modflow.TopElev_EIWell(i), '           -        Screen A top elevation '
    !        call Msg(TmpSTR)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBOffset_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBLength_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
    !        call Msg(TmpSTR)
    !
    !        WellFound=.false.
    !
    !        do j=1,Modflow.GWF.nCells/Modflow.GWF.nLayers  ! loop over the cells in layer 1
    !            if(Modflow.X_EIWell(i) >= Modflow.GWF.X(Modflow.GWF.iNode(1,j)) .and. Modflow.X_EIWell(i) <= Modflow.GWF.X(Modflow.GWF.iNode(4,j))) then
    !                if(Modflow.Y_EIWell(i) >= Modflow.GWF.Y(Modflow.GWF.iNode(1,j)) .and. Modflow.Y_EIWell(i) <= Modflow.GWF.Y(Modflow.GWF.iNode(2,j))) then
    !                    iCellCurr=j
    !                    WellFound=.true.
    !
    !                    write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', Modflow.GWF.xCell(iCellCurr), Modflow.GWF.yCell(iCellCurr)
    !                    call Msg(TmpSTR)
    !
    !                    write(TmpSTR,'(a,2f15.3)') 'X range ', Modflow.GWF.X(Modflow.GWF.iNode(1,j)), Modflow.GWF.X(Modflow.GWF.iNode(4,j))
    !                    call Msg(TmpSTR)
    !                    write(TmpSTR,'(a,2f15.3)') 'Y range ', Modflow.GWF.Y(Modflow.GWF.iNode(1,j)), Modflow.GWF.Y(Modflow.GWF.iNode(2,j))
    !                    call Msg(TmpSTR)
    !
    !
    !
    !                    call Msg(' Layer  Cell     Vertex         Z      Height')
    !                    write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,Modflow.GWF.Z(Modflow.GWF.iNode(4,iCellCurr))
    !                    call Msg(TmpSTR)
    !                    CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(4,iCellCurr))
    !                    do k=1,Modflow.GWF.nLayers
    !                        write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8,
    Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)),CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        call Msg(TmpSTR)
    !                        if(k==Modflow.GWF.nLayers) then
    !                            MeshBottom= Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        endif
    !                        CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                        iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                    end do
    !
    !                    iCellCurr=j
    !
    !
    !                    ! Top screen A
    !                    CurrTopElev=Modflow.TopElev_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenALength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    LayerLoop1: do k=1,Modflow.GWF.nLayers
    !                        if(CurrTopElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr A    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_A'
    !                            end if
    !                            if(CurrBotElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop1
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !
    !                                CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            if(iCellCurr > Modflow.GWF.nCells .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !
    !                        end if
    !                    end do LayerLoop1
    !
    !                    if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !
    !                    ! Screen B
    !                    CurrTopElev=CurrBotElev-Modflow.ScreenBOffset_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenBLength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    iCellCurr=j
    !                    LayerLoop2: do k=1,Modflow.GWF.nLayers
    !                        if(CurrTopElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr B    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_B'
    !                            endif
    !                            if(CurrBotElev > Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop2
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                CurrTopElev=Modflow.GWF.Z(Modflow.GWF.iNode(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.GWF.nCells/Modflow.GWF.nLayers
    !                            if(iCellCurr > Modflow.GWF.nCells .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !
    !                        end if
    !                    end do LayerLoop2
    !
    !                     if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !                end if
    !            end if
    !            if(WellFound) cycle WellSearch
    !        end do
    !    end do WellSearch
    !
    !     ! Modflow-USG CLN input file
    !    read(FNumMUT,'(a)') FnameCLN
    !    call OpenAscii(FnumCLN,FnameCLN)
    !    call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
    !
    !    ! Open new CLN well output file
    !    FNameCLNOut='out_'//FNameCLN
    !    call OpenAscii(FnumCLNOut,FNameCLNOut)
    !    call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
    !
    !    ! line 1 of CLN file
    !    read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,myNCONDUITYP
    !    iCLNNew=iCLNOrig+nEIScreens
    !    nSum=0
    !    do i=1,nEIScreens
    !        nSum=nSum+nCellList(i)
    !    end do
    !    iCellListNew=iCellListOrig+nSum
    !    write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,myNCONDUITYP
    !
    !    read(FnumCLN,'(a)') line
    !    write(FnumCLNOut,'(a)') line
    !
    !    do i=1,iCLNOrig
    !        read(FnumCLN,'(a)') line
    !        i2=index(line,'Well =')+6
    !        if(index(line(i2:),'IW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"')
    !        endif
    !        if(index(line(i2:),'EW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"')
    !        endif
    !
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNO')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2),
    trim(NameEIScreen(i))
    !    end do
    !
    !    do i=1,iCellListOrig
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNOD')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        do j=1,nCellList(i)
    !            !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !            write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !        end do
    !    end do
    !
    !    do i=1,iCLNOrig+myNCONDUITYP+2   ! myNCONDUITYP+2 lines then all starting heads
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    do i=1,nEIScreens  ! repeat last starting head for each new screen
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    ! in transport case may be more data so read/write to end of file
    !    do
    !        read(FnumCLN,'(a)',iostat=status) line
    !        if(status/=0) then
    !            exit
    !        endif
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !     ! Flowsource control file
    !    read(FNumMUT,'(a)') FnameFSCtl
    !    call OpenAscii(FNumFSCtl,FnameFSCtl)
    !    call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
    !
    !    ! Open new FlowSource control output file
    !    FnameFSCtlOut='out_'//FnameFSCtl
    !    call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
    !    call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
    !
    !    do
    !        read(FNumFSCtl,'(a)') line
    !        write(FNumFSCtlout,'(a)') line
    !        if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
    !            do i=1,nEIScreens,2
    !                if(index(NameEIScreen(i),'EW')>0) then
    !                    write(FNumFSCtlout,'(a)') ' '
    !                    write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
    !                    do j=1,nCellList(i)
    !                        write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
    !                    end do
    !                    do j=1,nCellList(i+1)
    !                        if(j==1) then
    !                            if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        else
    !                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        endif
    !                    end do
    !                end if
    !            end do
    !            exit
    !        end if
    !    end do
    !
    !    end_ctl: do
    !        read(FNumFSCtl,'(a)') line
    !        if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
    !            write(FNumFSCtlout,'(a)') ' '
    !            write(FNumFSCtlout,'(a)') line
    !
    !            do
    !                read(FNumFSCtl,'(a)',iostat=status) line
    !                if(status /= 0) then
    !                    exit end_ctl
    !                endif
    !
    !                write(FNumFSCtlout,'(a)') line
    !
    !            end do
    !        end if
    !    end do end_ctl
    !
    !end subroutine old_MUSG_PEST_EIWellCLNFileUpdate
    !
    !subroutine MUSG_PEST_CountParticlesToWells(FNumMUT) !--- Given modpath output file and CLN file info, count particles to each well and bin to arrival time.
    !    USE IFPORT
    !    implicit none
    !
    !    integer :: i, j, k, l
    !
    !    integer :: FNumMUT
    !
    !    integer :: FnumParticlesToWells
    !    character(MAXLBL) :: FNameParticlesToWells
    !
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !
    !    integer :: FnumEndpoint7
    !    character(MAXLBL) :: FNameEndpoint7
    !
    !    integer :: FnumEndpoint7_out
    !    character(MAXLBL) :: FNameEndpoint7_out
    !
    !    character(20), allocatable :: cln_WellName(:)
    !    integer, allocatable :: cln_nCells(:)
    !    integer, allocatable :: cln_CellList(:,:) ! up to 100 cells per CLN
    !
    !    integer, allocatable :: cln_nParticles(:,:)  ! number of particles reporting to this CLN(well),Bin(time)
    !
    !
    !    character(MAXLBL) :: line
    !    integer :: nCLN
    !    integer :: nTotCellList
    !    integer :: ifnod
    !    integer :: igwnod
    !    integer :: i1, i2, i3, i4, i5, i6
    !    real(dr) :: f1, f3, f4, f5, f6, f7, f8
    !
    !
    !    integer :: TrackingDirection
    !    integer :: TotalParticleCount
    !    integer :: iExit
    !    integer :: ICellStart
    !    integer :: ICellEnd
    !    real(dr) :: FinalTime
    !
    !    integer :: nBins
    !    real(dr), allocatable :: BinStart(:)
    !    real(dr), allocatable :: BinEnd(:)
    !
    !
    !   ! Paticles to wells information file
    !    read(FNumMUT,'(a)') FNameParticlesToWells
    !    call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
    !    call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
    !
    !
    !    read(FnumParticlesToWells,'(a)') FNameCLN
    !    call OpenAscii(FnumCLN,FNameCLN)
    !    call Msg( 'CLN file: '//trim(FNameCLN))
    !
    !
    !    read(FNumCLN,*) nCLN, i1, i2, i3, i4, i5, nTotCellList
    !    allocate(cln_WellName(nCLN), cln_nCells(nCLN), cln_CellList(NCLN,100))
    !    cln_nCells(:)=0
    !    cln_CellList(:,:)=0
    !
    !
    !    read(FNumCLN,'(a)') line   ! throw away line 2
    !    ! read CLN well names
    !    do i=1,nCLN
    !        read(FNumCLN,'(a)') line
    !        l1=index(line,'Well = ')
    !        if(l1>0) then
    !            read(line(l1+7:),'(a)') cln_WellName(i)
    !        endif
    !    end do
    !
    !    do i=1,nTotCellList
    !        read(FNumCLN,*) ifnod, igwnod
    !        cln_nCells(ifnod)=cln_nCells(ifnod)+1
    !        cln_CellList(ifnod,cln_nCells(ifnod))=igwnod
    !    end do
    !
    !    call FreeUnit(FNumCLN)
    !
    !    read(FnumParticlesToWells,*) nBins
    !    write(TMPStr,'(i8)') nBins
    !    call Msg('Number of bins: '//TMPStr)
    !    if(nBins>30) call ErrMsg('Current number of bins must be less than 30: '//TMPStr)
    !
    !
    !    allocate(BinStart(nBins), BinEnd(nBins))
    !    do i=1,nBins
    !        read(FnumParticlesToWells,*) BinStart(i),BinEnd(i)
    !        write(TMPStr,'(4g15.7)') BinStart(i),BinEnd(i)
    !        call Msg('Bin start, Bin end time: '//TMPStr)
    !
    !        if(BinStart(i)>=BinEnd(i)) then
    !            write(TMPStr,'(4f15.3)') BinStart(i),BinEnd(i)
    !            call ErrMsg('Bin start time >= Bin end time: '//TMPStr)
    !        endif
    !
    !        if(i>1) then
    !            do j=1,i-1
    !                if(BinStart(i) < BinEnd(j)) then
    !                    write(TMPStr,'(4f15.3)') BinStart(i), BinEnd(j)
    !                    call ErrMsg('Bin start time < Last bin end time: '//TMPStr)
    !                endif
    !            end do
    !        end if
    !    end do
    !
    !    allocate(cln_nParticles(nCLN,nBins))
    !    cln_nParticles(:,:)=0
    !
    !    do
    !        ! CLN file
    !        read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint7
    !        if(status /= 0) then
    !            exit
    !        endif
    !
    !        call OpenAscii(FnumEndpoint7,FNameEndpoint7)
    !        call Msg( 'Endpoint7 file: '//trim(FNameEndpoint7))
    !
    !        read(FnumEndpoint7,'(a)') line   ! check first line
    !        if(index(line,'MODPATH_ENDPOINT_FILE') == 0) then
    !            call Msg( 'Not a valid Modpath7 endpoint file')
    !            stop
    !        endif
    !
    !        read(FnumEndpoint7,*) TrackingDirection, TotalParticleCount
    !        read(FnumEndpoint7,'(a)') line
    !        read(FnumEndpoint7,'(a)') line
    !        read(FnumEndpoint7,'(a)') line
    !        read(FnumEndpoint7,'(a)') line
    !        Particle: do i=1, TotalParticleCount
    !            read(FnumEndpoint7,*) i1, i2, i3, iExit, f1, FinalTime, iCellStart, i4, f3, f4, f5, f6, f7, f8, i5, i6, iCellEnd
    !            do k=1,nCLN
    !                do j=1,cln_nCells(k)
    !                    if(iCellEnd==cln_CellList(k,j) .and. iExit==5) then
    !                        do l=1,nBins
    !                            if(FinalTime >= BinStart(l) .and. FinalTime < BinEnd(l)) then
    !                                cln_nParticles(k,l)=cln_nParticles(k,l)+1
    !                                cycle Particle
    !                            endif
    !                        end do
    !                        write(TMPStr,'(4f15.3)') FinalTime
    !                        call ErrMsg('FinalTime fall outside of bin range: '//TMPStr)
    !                    endif
    !                end do
    !            end do
    !        end do Particle
    !
    !        ! Write particle count file
    !        FNameEndpoint7_out=trim(FNameEndpoint7)//'_PToWell.csv'
    !        call OpenAscii(FnumEndpoint7_out,FNameEndpoint7_out)
    !        call Msg( 'Endpoint7 particle count file: '//trim(FNameEndpoint7_out))
    !        write(FnumEndpoint7_out,'(a,30(i8,a))') 'WellName,#ParticlesReportingToBin',(i,',',i=1,nBins)
    !        do i=1,nCLN
    !            write(FnumEndpoint7_out,'(a20,a,30(i8,a))') cln_WellName(i), ',',(cln_nParticles(i,j),',',j=1,nBins)
    !        end do
    !
    !        call FreeUnit(FnumEndpoint7)
    !        call FreeUnit(FnumEndpoint7_out)
    !
    !    end do
    !
    !end subroutine MUSG_PEST_CountParticlesToWells
    !
    !subroutine M2005_PEST_CountParticlesToWells(FNumMUT) !--- Given modpath5 output file and ??? file info, count particles to each well and bin to arrival time.
    !    USE IFPORT
    !    implicit none
    !
    !    integer :: i, j, k
    !
    !    integer :: FNumMUT
    !
    !    integer :: FnumParticlesToWells
    !    character(MAXLBL) :: FNameParticlesToWells
    !
    !    integer :: FnumMNW
    !    character(MAXLBL) :: FNameMNW
    !
    !    integer :: FNumEndpoint5
    !    character(MAXLBL) :: FNameEndpoint5
    !
    !    integer :: FNumEndpoint5_out
    !    character(MAXLBL) :: FNameEndpoint5_out
    !
    !    integer :: nLines
    !
    !    character(20), allocatable :: WellName(:)
    !    integer, allocatable :: nIJKs(:)
    !    integer, allocatable :: IJKList(:,:,:) ! up to 100 IJKs per mnw
    !    integer :: iRow, jCol, kLyr
    !
    !    integer, allocatable :: nParticles(:)  ! number of particles reporting to this MNW(well),Bin(time)
    !
    !
    !    character(MAXLBL) :: line
    !    integer :: nWells
    !
    !    integer :: i1
    !
    !   ! Paticles to wells information file
    !    read(FNumMUT,'(a)') FNameParticlesToWells
    !    call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
    !    call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
    !
    !    read(FnumParticlesToWells,'(a)') FNameMNW
    !    call OpenAscii(FnumMNW,FNameMNW)
    !    call Msg( 'MNW file: '//trim(FNameMNW))
    !
    !    read(FNumMNW,'(a)') line   ! throw away line 1
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    read(FNumMNW,*) nLines
    !    ! count wells
    !    nWells=0
    !    do i=1,nLines
    !        read(FNumMNW,'(a)') line
    !        if(index(line,'SITE:') > 1) nWells=nWells+1
    !    end do
    !
    !    allocate(WellName(nWells), nIJKs(nWells), IJKList(nWells,3,100))
    !    nIJKs(:)=0
    !    IJKList(:,:,:)=0
    !
    !    continue
    !
    !    rewind(FNumMNW)
    !    read(FNumMNW,'(a)') line   ! throw away line 1
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    ! read well name and ijk's
    !    nWells=0
    !    do i=1,nLines
    !        read(FNumMNW,'(a)') line
    !        if(index(line,'SITE:') > 1) then
    !            nWells=nWells+1
    !            nIJKs(nWells)=nIJKs(nWells)+1
    !            read(line,*) kLyr, iRow, jCol
    !            IJKList(nWells,1,nIJKs(nWells))=iRow
    !            IJKList(nWells,2,nIJKs(nWells))=jCol
    !            IJKList(nWells,3,nIJKs(nWells))=kLyr
    !            l1=index(line,'SITE:')
    !            WellName(nWells)=line(l1+5:)
    !        else
    !            nIJKs(nWells)=nIJKs(nWells)+1
    !            read(line,*) kLyr, iRow, jCol
    !            IJKList(nWells,1,nIJKs(nWells))=iRow
    !            IJKList(nWells,2,nIJKs(nWells))=jCol
    !            IJKList(nWells,3,nIJKs(nWells))=kLyr
    !        end if
    !    end do
    !    call FreeUnit(FNumMNW)
    !
    !    allocate(nParticles(nWells))
    !
    !    do
    !        ! Particles file
    !        read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint5
    !        if(status /= 0) then
    !            exit
    !        endif
    !
    !        call OpenAscii(FNumEndpoint5,FNameEndpoint5)
    !        call Msg( 'Endpoint5 file: '//trim(FNameEndpoint5))
    !
    !        read(FNumEndpoint5,'(a)') line   ! check first line
    !        if(index(line,'@ [ MODPATH 5.0') == 0) then
    !            call Msg( 'Not a Modpath 5 endpoint file')
    !            stop
    !        endif
    !
    !        nParticles(:)=0
    !        Particle: do
    !            read(FNumEndpoint5,*,iostat=status) i1, jCol, iRow, kLyr
    !            if(status/=0) exit Particle
    !
    !            do k=1,nWells
    !                do j=1,nIJKs(k)
    !                    if(IJKList(k,1,j)==iRow .and. IJKList(k,2,j)==jCol .and. IJKList(k,3,j)==kLyr) then
    !                        nParticles(k)=nParticles(k)+1
    !                        cycle Particle
    !                    endif
    !                end do
    !            end do
    !        end do Particle
    !
    !        ! Write particle count file
    !        FNameEndpoint5_out=trim(FNameEndpoint5)//'_PToWell.csv'
    !        call OpenAscii(FNumEndpoint5_out,FNameEndpoint5_out)
    !        call Msg( 'Endpoint5 particle count file: '//trim(FNameEndpoint5_out))
    !        write(FNumEndpoint5_out,'(a,30(i8,a))') 'WellName,#ParticlesReporting'
    !        do i=1,nWells
    !            write(FNumEndpoint5_out,'(a20,a,30(i8,a))') WellName(i), ',',nParticles(i)
    !        end do
    !
    !        call FreeUnit(FNumEndpoint5)
    !        call FreeUnit(FNumEndpoint5_out)
    !
    !    end do
    !
    !end subroutine M2005_PEST_CountParticlesToWells
    !
    !subroutine MUSG_PEST_RTWellOptimization(FNumMUT) !--- Given RT pumping well info, update well .WEL file
    !    !USE IFPORT
    !    implicit none
    !
    !    !type (ModflowProject) Modflow
    !
    !
    !    integer :: i, j
    !
    !    integer :: FNumMUT
    !
    !    integer :: FnumRTRates
    !    character(MAXLBL) :: FnameRTRates
    !
    !    integer :: FnumRTOnOff
    !    character(MAXLBL) :: FNameRTOnOff
    !    integer :: FnumRTOnOffNints
    !    character(MAXLBL) :: FNameRTOnOffNints
    !
    !    integer :: FnumRTRateOut
    !    character(MAXLBL) :: FnameRTRateOut
    !
    !    integer :: FNumWel
    !    character(MAXLBL) :: FnameWel
    !    integer :: FNumWelOut
    !    character(MAXLBL) :: FnameWelOut
    !
    ! !   integer, allocatable :: nCellList(:)
	   ! !character(31), allocatable :: NameEIScreen(:)
    ! !   integer, allocatable :: CellNumber(:,:)
    ! !   real(dr), allocatable :: CellScreenLength(:,:)
    ! !
    !
    !    integer :: i1, i2
    !    character(MAXSTRING) :: line
    !
    !    character(31) :: WellName(MAXCLN)
    !    integer :: RTWell_CLN_Number
    !    real(dr) :: HighRate_ft3_day(MAXCLN)
    !    real(dr) :: LowRate_ft3_day(MAXCLN)
    !
    !    integer :: nRTOnOff
    !    integer, allocatable :: cln_number_On_Off(:)
    !    integer, allocatable :: SP_num_On_Off(:)
    !    real(dr), allocatable :: On_off(:)
    !    real(dr),allocatable :: AppliedRate_ft3_day(:)
    !    real(dr) :: State
    !    integer, allocatable :: indx_col(:)
    !
    !    integer :: iStressperiod
    !    integer :: nStressperiods
    !    integer :: IWellOrig
    !    integer :: IWellNew(MAXSTRESS)
    !    integer :: idum1, idum2
    !    real(dr) :: RTRateTotal(MAXSTRESS)
    !
    !    integer :: iOnOff
    !    integer :: iCLN
    !
    !    character(MAXSTRING) :: CMDString
    !
    !
    !    integer :: FnumLADWP
    !    character(MAXLBL) :: FnameLADWP
    !    real(dr) :: LADWPRateTotal(MAXSTRESS)
    !    real(dr) :: LADWPRatio
    !    real(dr) :: LADWPPenalty
    !
    !
    !    integer :: FnumPenalty
    !    character(MAXLBL) :: FNamePenalty
    !    integer :: nRange
    !    real(dr), allocatable :: MinPenalty(:)
    !    real(dr), allocatable :: MaxPenalty(:)
    !    real(dr), allocatable :: MinRange(:)
    !    real(dr), allocatable :: MaxRange(:)
    !
    !    !-----------------------------------------------------------------------------------
    !    ! RT wells rate file
    !    read(FNumMUT,'(a)') FnameRTRates
    !    call OpenAscii(FnumRTRates,FnameRTRates)
    !    call Msg( 'RT well rate input file: '//trim(FnameRTRates))
    !
    !    ! Read header
    !    read(FnumRTRates,'(a)') line
    !    do
    !        read(FnumRTRates,'(a)',iostat=status) line
    !        if(status/=0) exit
    !
    !        l1=index(line,',')
    !        read(line(:l1-1),'(a)') TMPStr
    !
    !        line=line(l1+1:)
    !        l1=index(line,',')
    !        read(line(:l1),*) RTWell_CLN_Number
    !
    !        WellName(RTWell_CLN_Number)=TMPStr
    !
    !        line=line(l1+1:)
    !        read(line,*) HighRate_ft3_day(RTWell_CLN_Number)
    !
    !        l1=index(line,',')
    !        line=line(l1+1:)
    !        read(line,*) LowRate_ft3_day(RTWell_CLN_Number)
    !
    !    end do
    !
    !    continue
    !
    !    !-----------------------------------------------------------------------------------
    !    ! RT wells on/off file
    !    read(FNumMUT,'(a)') FNameRTOnOff
    !    call OpenAscii(FNumRTOnOff,FNameRTOnOff)
    !    call Msg( 'RT well on/off input file: '//trim(FNameRTOnOff))
    !
    !    FNameRTOnOffNints='AppliedRates.txt'
    !    call OpenAscii(FNumRTOnOffNints,FNameRTOnOffNints)
    !    call Msg( 'RT well on/off input file: '//trim(FNameRTOnOffNints))
    !
    !    ! Read header
    !    read(FNumRTOnOff,'(a)') line
    !    write(FNumRTOnOffNints,'(a)') '   CLN,Stress period,    On_off, State,  Applied rate (ft3/day)'
    !
    !    nRTOnOff=0
    !    do
    !        read(FNumRTOnOff,'(a)',iostat=status) line
    !        if(status/=0) exit
    !
    !        nRTOnOff=nRTOnOff+1
    !
    !    end do
    !
    !    allocate(cln_number_On_Off(nRTOnOff), SP_num_On_Off(nRTOnOff), On_off(nRTOnOff), indx_col(nRTOnOff), AppliedRate_ft3_day(nRTOnOff))
    !
    !    rewind(FNumRTOnOff)
    !    ! Read header
    !    read(FNumRTOnOff,'(a)') line
    !    do i=1, nRTOnOff
    !        read(FNumRTOnOff,*) cln_number_On_Off(i), SP_num_On_Off(i), On_off(i)
    !
    !        ! Compute applied rate from inputs
    !        if(LowRate_ft3_day(cln_number_On_Off(i))==-9999.) then  ! on/off state from nint
    !            if(nint(On_off(i))==0) then
    !                AppliedRate_ft3_day(i)=0.0d0
    !                State=0.0
    !            else if(nint(On_off(i))==1) then
    !                AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
    !                State=1.0
    !            else
    !                call ErrMsg('Nint not 0 or 1')
    !            endif
    !        else if(LowRate_ft3_day(cln_number_On_Off(i))/=-9999.) then  ! off/low/high state from fraction
    !            if(On_off(i) >= 0.0 .and. On_off(i) < 0.333) then
    !                AppliedRate_ft3_day(i)=0.0d0
    !                State=0.0
    !            else if(On_off(i) >= 0.333 .and. On_off(i) < 0.666) then
    !                AppliedRate_ft3_day(i)=LowRate_ft3_day(cln_number_On_Off(i))
    !                State=0.5
    !            else if(On_off(i) >= 0.666 .and. On_off(i) <= 1.0) then
    !                AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
    !                State=1.0
    !            else
    !                call ErrMsg('Nint not in range between 0.0 and 1.0')
    !
    !            endif
    !        else
    !            call ErrMsg('Bad low rate')
    !        endif
    !        write(FNumRTOnOffNints,'(i6,i6,f20.5,f5.1,f20.5)') cln_number_On_Off(i), SP_num_On_Off(i), On_off(i),State,AppliedRate_ft3_day(i)
    !    end do
    !
    !
    !    !-----------------------------------------------------------------------------------
    !    ! Pass1: Read/write well file and update RT well rate, inserting well if not present
    !    !    - count new total wells for stress period
    !
    !    ! Modflow Well .wel file
    !    read(FNumMUT,'(a)') FnameWel
    !    call OpenAscii(FNumWel,FnameWel)
    !    call Msg( 'Modflow well file: '//trim(FnameWel))
    !
    !    ! Open new well .wel output file
    !    FnameWelOut='pass1.tmp'
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output TEMP file: '//trim(FnameWelOut))
    !
    !    read(FNumWel,'(a)') line
    !    write(FnumWelOut,'(a)') trim(line)  ! save first line
    !
    !    iOnOff=1
    !    RTRateTotal(:)= 0.0
    !    nStressPeriods=0
    !    read_wel_file: do
    !        read(FNumWel,'(a)',iostat=status) line
    !        if(status /= 0) then  ! end of file
    !            exit read_wel_file
    !        endif
    !
    !        l1=index(line,'stress period ')
    !        if(l1 > 0) then ! new stress period
    !            write(FnumWelOut,'(a)') trim(line)  ! save stress period header
    !
    !            ! Extract stress period from line
    !            l1=l1+14  ! position at end of string 'stress period '
    !            TMPStr=line(31:)
    !            l2=l1+index(line(l1:),':')-2
    !            read(line(l1:l2),*) iStressPeriod
    !
    !            nStressPeriods=max(nStressPeriods,iStressPeriod)
    !
    !
    !            read(line,*) idum1, idum2, iWellOrig
    !            iWellNew(iStressPeriod)=iWellOrig
    !            do
    !                read(FNumWel,'(a)',iostat=status) line
    !                if(status/=0) exit read_wel_file
    !                l1=index(line,'stress period ')
    !                if(l1 > 0) then ! next stress period
    !                    backspace(FNumWel)
    !                    cycle read_wel_file
    !                endif
    !
    !
    !                read(line,*) iCLN
    !
    !                if( iOnOff<=nRTOnOff) then
    !                    if(SP_num_On_Off(iOnOff)==iStressPeriod) then
    !                        if(iCLN<cln_number_On_Off(iOnOff)) then ! NON RT CLN
    !                            write(FnumWelOut,'(a)') trim(line)
    !                        else if(iCLN==cln_number_On_Off(iOnOff)) then  ! RT CLN already in list
    !                            !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'overwrite ',
    iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),trim(WellName(cln_number_On_Off(iOnOff)))
    !                            i1=27
    !                            i2=index(line,'well =')+6
    !                            write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
    !                            RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
    !                            iOnOff=iOnOff+1
    !                        else if(iCLN>cln_number_On_Off(iOnOff)) then  ! RT CLN not in list
    !                            !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'add ', iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),WellName(cln_number_On_Off(iOnOff))
    !                            i1=27
    !                            i2=index(line,'well =')+6
    !                            write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
    !                            RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
    !                            iOnOff=iOnOff+1
    !                            iWellNew(iStressPeriod)=iWellNew(iStressPeriod)+1
    !                            backspace(FnumWel)
    !                            !write(FnumWelOut,'(a,i5)') 'inc nwells ', iWellNew(iStressPeriod)
    !                        endif
    !                        !iOnOff=min(iOnOff,nRTOnOff)
    !                    else
    !                        write(FnumWelOut,'(a)') trim(line)
    !                    end if
    !                else
    !                    write(FnumWelOut,'(a)') trim(line)
    !                end if
    !
    !
    !            end do
    !            continue
    !
    !        endif
    !    end do read_wel_file
    !
    !    call freeunit(FNumWel)
    !    call freeunit(FNumWelOut)
    !
    !    ! Pass2: update number of wells per stress period
    !    call OpenAscii(FNumWel,'pass1.tmp')
    !
    !    ! Open new well .wel output file
    !    FnameWelOut='rt_'//FnameWel
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output file: '//trim(FnameWelOut))
    !
    !    do
    !        read(FnumWel,'(a)',iostat=status) line
    !        if(status /= 0) then  ! end of file
    !            exit
    !        endif
    !
    !        l1=index(line,'stress period ')
    !        if(l1 > 0) then ! new stress period, adjust number of wells
    !
    !            ! Extract stress period from line
    !            l1=l1+14  ! position at end of string 'stress period '
    !            TMPStr=line(31:)
    !            l2=l1+index(line(l1:),':')-2
    !            read(line(l1:l2),*) iStressPeriod
    !
    !            read(line,*) idum1, idum2, iWellOrig
    !            write(line(:30),'(3i10)') idum1, idum2,iWellNew(iStressPeriod)
    !
    !        endif
    !
    !        write(FnumWelOut,'(a)') trim(line)
    !
    !    end do
    !
    !    call freeunit(FNumWel)
    !    call freeunit(FNumWelOut)
    !    CMDString='del pass1.tmp'
    !    i=system(CMDString)
    !
    !
    !    ! LADPW rates input file
    !    read(FNumMUT,'(a)') FnameLADWP
    !    call OpenAscii(FnumLADWP,FnameLADWP)
    !    call Msg( 'LADPW rate input file: '//trim(FnameLADWP))
    !
    !    read(FnumLADWP,'(a)') line
    !    do i=1,nStressPeriods
    !        read(FnumLADWP,*) j, LADWPRateTotal(i)
    !    end do
    !
    !    ! Penalties input file
    !    read(FNumMUT,'(a)') FNamePenalty
    !    call OpenAscii(FNumPenalty,FNamePenalty)
    !    call Msg( 'LADPW rate penalties file: '//trim(FNamePenalty))
    !
    !    read(FNumPenalty,*) nRange
    !    allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
    !    do i=1,nRange
    !        read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
    !        write(TMPStr,'(4f20.5)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
    !        call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
    !        if(i>1) then
    !            if(Minrange(i)/=MaxRange(i-1)) then
    !                write(ErrStr,'(a,2f20.5)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
    !                call Msg(ErrStr)
    !                write(ErrStr,'(a,2f20.5)') 'Range 2: ', MinRange(i), MaxRange(i)
    !                call Msg(ErrStr)
    !                call ErrMsg('Min range 2 not equal to max range 1')
    !            end if
    !            if(MinPenalty(i)/=MaxPenalty(i-1)) then
    !                write(ErrStr,'(a,2f20.5)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
    !                call Msg(ErrStr)
    !                write(ErrStr,'(a,2f20.5)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
    !                call Msg(ErrStr)
    !                call ErrMsg('Min penalty 2 not equal to max penalty 1')
    !            end if
    !        end if
    !    end do
    !
    !
    !    ! Open RT well rate output file
    !    FnameRTRateOut='rt_rates_per_SP.lst'
    !    call OpenAscii(FnumRTRateOut,FnameRTRateOut)
    !    call Msg( 'RT well rate output file: '//trim(FnameRTRateOut))
    !    write(FnumRTRateOut,'(a)') 'Stress period,RT Well Rate Total, LADPW Design Rate,       RT:LADWP Ratio,     PEST Penalty'
    !    do i=1,nStressPeriods
    !        LADWPRatio=RTRateTotal(i)/LADWPRateTotal(i)
    !        do j=1,nrange
    !            if(LADWPRatio>= MinRange(j) .and. LADWPRatio<= MaxRange(j)) then ! linear interpolation for penalty
    !                LADWPPenalty= MinPenalty(j)+(LADWPRatio-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
    !            endif
    !        end do
    !        write(FnumRTRateOut,'(i10,4f20.5)') i,RTRateTotal(i),LADWPRateTotal(i),LADWPRatio,LADWPPenalty
    !    end do
    !
    !    continue
    !
    !
    !end subroutine MUSG_PEST_RTWellOptimization
