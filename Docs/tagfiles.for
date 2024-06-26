    character(60) :: MUSG_TagFiles_CMD='tag modflow files'

            else if(index(MUSG_CMD, MUSG_TagFiles_CMD)  /= 0) then
                Modflow.TagFiles=.true.



        if(Modflow.TagFiles) then
            call MUSG_TagSMSFile(Modflow)
        end if

    subroutine MUSG_TagSMSFile(Modflow)
        implicit none

        character(200) :: line
        character(200) :: opt(20)='none'
        integer :: i, io_tagged
        integer :: nOpt=0
        REAL ::HCLOSE, HICLOSE
        integer :: MXITER, ITER1, IPRSMS, NONLINMETH, LINMETH
        real :: THETA, AKAPPA, GAMMA,AMOMENTUM,BTOL,BREDUC,RESLIM,RRCTOL,EPSRN,RCLOSEPCGU,RELAXPCGU
        character(20) :: CLIN
        integer :: NUMTRACK,IACL,NORDER,LEVEL,NORTH,IREDSYS,IDROPTOL,IPC,ISCL,IORD


        type (ModflowProject) Modflow

        FName=trim(Modflow.FNameSMS)//'_tagged'
        call Msg('New SMS file: '//trim(FName))
        call getunit(io_tagged)
        open(io_tagged,file=FName,status='unknown',form='formatted')

        write(io_tagged,'(a,1pg10.1)') '# This file tagged by Modflow-User-Tools version ',MUTVersion


        rewind(Modflow.iSMS)
        do
            read(Modflow.iSMS,'(a)',iostat=status) line
            if(status/=0) then
                call ErrMsg('Unexpected end of SMS file')
            end if

            if(line(1:1)=='#') then
                !write(io_tagged,'(a)') line
                cycle
            else if(index(line,'SIMPLE') >0 .or. index(line,'MODERATE')  .or. index(line,'COMPLEX')) then
                write(io_tagged,'(a)') '#----------------------------------------------------------------------------------------'
                write(io_tagged,'(a)') '1a. OPTIONS'
                nOpt=1
                write(io_tagged,'(a)') line
                cycle
            else
                write(io_tagged,'(a)') '#------------------------------------------------------------------------------------------'
                write(io_tagged,'(a)') '#1b.     HCLOSE   HICLOSE      MXITER    ITER1     IPRSMS  NONLINMETH  LINMETH   Options...'
                read(line,*,end=10) HCLOSE, HICLOSE, MXITER, ITER1, IPRSMS, NONLINMETH, LINMETH, (opt(i),i=1,20)
10              write(TmpSTR,'(5x,2(1pg10.1),5(i10),10x)') HCLOSE, HICLOSE, MXITER, ITER1, IPRSMS, NONLINMETH, LINMETH

                do i=1,20
                    if(opt(i) /= 'none') then
                        TmpSTR=trim(TmpSTR)//'      '//trim(opt(i))
                    end if
                end do
                write(io_tagged,'(a)') trim(TmpSTR)

                write(io_tagged,'(a)') '# HCLOSE—is the head change criterion for convergence of the outer (nonlinear)'
                write(io_tagged,'(a)') '#    iterations, in units of length. When the maximum absolute value of the head'
                write(io_tagged,'(a)') '#    change at all nodes during an iteration is less than or equal to HCLOSE,'
                write(io_tagged,'(a)') '#    iteration stops. Commonly, HCLOSE equals 0.01.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#HICLOSE—is the head change criterion for convergence of the inner (linear)'
                write(io_tagged,'(a)') '#    iterations, in units of length. When the maximum absolute value of the head'
                write(io_tagged,'(a)') '#    change at all nodes during an iteration is less than or equal to HICLOSE, the'
                write(io_tagged,'(a)') '#    matrix solver assumes convergence. Commonly, HICLOSE is set an order of'
                write(io_tagged,'(a)') '#    magnitude less than HCLOSE.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#MXITER—is the maximum number of outer (nonlinear) iterations -- that is,'
                write(io_tagged,'(a)') '#    calls to the solution routine. For a linear problem MXITER should be 1.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    ITER1—is the maximum number of inner (linear) iterations. The number'
                write(io_tagged,'(a)') '#    typically depends on the characteristics of the matrix solution scheme being'
                write(io_tagged,'(a)') '#    used. For nonlinear problems,'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    ITER1 usually ranges from 60 to 600; a value of 100 will be sufficient for'
                write(io_tagged,'(a)') '#    most linear problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#IPRSMS—is a flag that controls printing of convergence information from the solver:'
                write(io_tagged,'(a)') '#    0 – print nothing'
                write(io_tagged,'(a)') '#    1 – print only the total number of iterations and nonlinear residual reduction summaries'
                write(io_tagged,'(a)') '#    2 – print matrix solver information in addition to above'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#NONLINMETH—is a flag that controls the nonlinear solution method and under-relaxation schemes'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    0 – Picard iteration scheme is used without any under-relaxation schemes involved'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    > 0 – Newton-Raphson iteration scheme is used with under-relaxation. Note'
                write(io_tagged,'(a)') '#    that the Newton-Raphson linearization scheme is available only for the'
                write(io_tagged,'(a)') '#    upstream weighted solution scheme of the BCF and LPF packages.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    < 0 – Picard iteration scheme is used with under-relaxation.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    The absolute value of NONLINMETH determines the underrelaxation scheme used.'
                write(io_tagged,'(a)') '#    1 or -1 – Delta-Bar-Delta under-relaxation is used.'
                write(io_tagged,'(a)') '#    2 or -2 – Cooley under-relaxation scheme is used.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#    Note that the under-relaxation schemes are used in conjunction with'
                write(io_tagged,'(a)') '#    gradient based methods, however, experience has indicated that the Cooley'
                write(io_tagged,'(a)') '#    under-relaxation and damping work well also for the Picard scheme with'
                write(io_tagged,'(a)') '#    the wet/dry options of MODFLOW.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#LINMETH—is a flag that controls the matrix solution method'
                write(io_tagged,'(a)') '#    1 – the χMD solver of Ibaraki (2005) is used.'
                write(io_tagged,'(a)') '#    2 – the unstructured pre-conditioned conjugate gradient solver of White'
                write(io_tagged,'(a)') '#    and Hughes (2011) is used.'
                write(io_tagged,'(a)') '#'
                exit
            end if

        end do

        do
            read(Modflow.iSMS,'(a)',iostat=status) line
            if(status/=0) then
                call ErrMsg('Unexpected end of SMS file')
            end if

            if(line(1:1)=='#') then
                !write(io_tagged,'(a)') line
                cycle
            else
                exit
            end if
        end do

        if(NONLINMETH /= 0 .and. nOpt==0) then
            write(io_tagged,'(a)') '#----------------------------------------------------------------------------------------'
            write(io_tagged,'(a)') '#2.    THETA     AKAPPA      GAMMA     AMOMENTUM    NUMTRACK  BTOL      BREDUC     RESLIM'

            read(line,*) THETA, AKAPPA, GAMMA, AMOMENTUM, NUMTRACK

            if(NUMTRACK > 0) then
                read(line,*) THETA, AKAPPA, GAMMA, AMOMENTUM, NUMTRACK,BTOL, BREDUC, RESLIM

                write(TmpSTR,'(5x,4(1pg10.1,1x),1(i10,1x),4(1pg10.1,1x))') THETA, AKAPPA, GAMMA, AMOMENTUM, NUMTRACK, BTOL, BREDUC, RESLIM
                write(io_tagged,'(a)') trim(TmpSTR)

                write(io_tagged,'(a)') '#THETA—is the reduction factor for the learning rate (under-relaxation term)'
                write(io_tagged,'(a)') '#    of the delta-bar-delta algorithm. The value of THETA is between zero and one.'
                write(io_tagged,'(a)') '#    If the change in the variable (head) is of opposite sign to that of the'
                write(io_tagged,'(a)') '#    previous iteration, the under-relaxation term is reduced by a factor of'
                write(io_tagged,'(a)') '#    THETA. The value usually ranges from 0.3 to 0.9; a value of 0.7 works well'
                write(io_tagged,'(a)') '#    for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#AKAPPA—is the increment for the learning rate (under-relaxation term) of the'
                write(io_tagged,'(a)') '#    delta-bar-delta algorithm. The value of AKAPPA is between zero and one. If'
                write(io_tagged,'(a)') '#    the change in the variable (head) is of the same sign to that of the previous'
                write(io_tagged,'(a)') '#    iteration, the under-relaxation term is increased by an increment of AKAPPA.'
                write(io_tagged,'(a)') '#    The value usually ranges from 0.03 to 0.3; a value of 0.1 works well for most'
                write(io_tagged,'(a)') '#    problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#GAMMA—is the history or memory term factor of the delta-bar-delta algorithm.'
                write(io_tagged,'(a)') '#    Gamma is between zero and 1 but cannot be equal to one. When GAMMA is zero,'
                write(io_tagged,'(a)') '#    only the most recent history (previous iteration value) is maintained. As'
                write(io_tagged,'(a)') '#    GAMMA is increased, past history of iteration changes has greater influence'
                write(io_tagged,'(a)') '#    on the memory term. The memory term is maintained as an exponential average'
                write(io_tagged,'(a)') '#    of past changes. Retaining some past history can overcome granular behavior'
                write(io_tagged,'(a)') '#    in the calculated function surface and therefore helps to overcome cyclic'
                write(io_tagged,'(a)') '#    patterns of non-convergence. The value usually ranges from 0.1 to 0.3; a'
                write(io_tagged,'(a)') '#    value of 0.2 works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#AMOMENTUM—is the fraction of past history changes that is added as a momentum'
                write(io_tagged,'(a)') '#    term to the step change for a nonlinear iteration. The value of AMOMENTUM is'
                write(io_tagged,'(a)') '#    between zero and one. A large momentum term should only be used when small'
                write(io_tagged,'(a)') '#    learning rates are expected. Small amounts of the momentum term help'
                write(io_tagged,'(a)') '#    convergence. The value usually ranges from 0.0001 to 0.1; a value of 0.001'
                write(io_tagged,'(a)') '#    works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#NUMTRACK—is the maximum number of backtracking iterations allowed for'
                write(io_tagged,'(a)') '#    residual reduction computations. If NUMTRACK = 0 then the backtracking'
                write(io_tagged,'(a)') '#    iterations are omitted. The value usually ranges from 2 to 20; a value of 10'
                write(io_tagged,'(a)') '#    works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#BTOL—is the tolerance for residual change that is allowed for residual'
                write(io_tagged,'(a)') '#    reduction computations. BTOL should not be less than one to avoid getting'
                write(io_tagged,'(a)') '#    stuck in local minima. A large value serves to check for extreme residual'
                write(io_tagged,'(a)') '#    increases, while a low value serves to control step size more severely. The'
                write(io_tagged,'(a)') '#    value usually ranges from 1.0 to 106; a value of 104 works well for most'
                write(io_tagged,'(a)') '#    problems but lower values like 1.1 may be required for harder problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#BREDUC—is the reduction in step size used for residual reduction'
                write(io_tagged,'(a)') '#    computations. The value of BREDUC is between zero and one. The value usually'
                write(io_tagged,'(a)') '#    ranges from 0.1 to 0.3; a value of 0.2 works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#RESLIM—is the limit to which the residual is reduced with backtracking. If'
                write(io_tagged,'(a)') '#    the residual is smaller than RESLIM, then further backtracking is not'
                write(io_tagged,'(a)') '#    performed. A value of 100 is suitable for large problems and residual'
                write(io_tagged,'(a)') '#    reduction to smaller values may only slow down computations.'
                write(io_tagged,'(a)') '#'
            else
                write(TmpSTR,'(5x,4(1pg10.1,1x),1(i10,1x),4(1pg10.1,1x))') THETA, AKAPPA, GAMMA, AMOMENTUM, NUMTRACK
                write(io_tagged,'(a)') trim(TmpSTR)

                write(io_tagged,'(a)') '#THETA—is the reduction factor for the learning rate (under-relaxation term)'
                write(io_tagged,'(a)') '#    of the delta-bar-delta algorithm. The value of THETA is between zero and one.'
                write(io_tagged,'(a)') '#    If the change in the variable (head) is of opposite sign to that of the'
                write(io_tagged,'(a)') '#    previous iteration, the under-relaxation term is reduced by a factor of'
                write(io_tagged,'(a)') '#    THETA. The value usually ranges from 0.3 to 0.9; a value of 0.7 works well'
                write(io_tagged,'(a)') '#    for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#AKAPPA—is the increment for the learning rate (under-relaxation term) of the'
                write(io_tagged,'(a)') '#    delta-bar-delta algorithm. The value of AKAPPA is between zero and one. If'
                write(io_tagged,'(a)') '#    the change in the variable (head) is of the same sign to that of the previous'
                write(io_tagged,'(a)') '#    iteration, the under-relaxation term is increased by an increment of AKAPPA.'
                write(io_tagged,'(a)') '#    The value usually ranges from 0.03 to 0.3; a value of 0.1 works well for most'
                write(io_tagged,'(a)') '#    problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#GAMMA—is the history or memory term factor of the delta-bar-delta algorithm.'
                write(io_tagged,'(a)') '#    Gamma is between zero and 1 but cannot be equal to one. When GAMMA is zero,'
                write(io_tagged,'(a)') '#    only the most recent history (previous iteration value) is maintained. As'
                write(io_tagged,'(a)') '#    GAMMA is increased, past history of iteration changes has greater influence'
                write(io_tagged,'(a)') '#    on the memory term. The memory term is maintained as an exponential average'
                write(io_tagged,'(a)') '#    of past changes. Retaining some past history can overcome granular behavior'
                write(io_tagged,'(a)') '#    in the calculated function surface and therefore helps to overcome cyclic'
                write(io_tagged,'(a)') '#    patterns of non-convergence. The value usually ranges from 0.1 to 0.3; a'
                write(io_tagged,'(a)') '#    value of 0.2 works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#AMOMENTUM—is the fraction of past history changes that is added as a momentum'
                write(io_tagged,'(a)') '#    term to the step change for a nonlinear iteration. The value of AMOMENTUM is'
                write(io_tagged,'(a)') '#    between zero and one. A large momentum term should only be used when small'
                write(io_tagged,'(a)') '#    learning rates are expected. Small amounts of the momentum term help'
                write(io_tagged,'(a)') '#    convergence. The value usually ranges from 0.0001 to 0.1; a value of 0.001'
                write(io_tagged,'(a)') '#    works well for most problems.'
                write(io_tagged,'(a)') '#'
                write(io_tagged,'(a)') '#NUMTRACK—is the maximum number of backtracking iterations allowed for'
                write(io_tagged,'(a)') '#    residual reduction computations. If NUMTRACK = 0 then the backtracking'
                write(io_tagged,'(a)') '#    iterations are omitted. The value usually ranges from 2 to 20; a value of 10'
                write(io_tagged,'(a)') '#    works well for most problems.'
                write(io_tagged,'(a)') '#'
            end if
        end if

        do
            read(Modflow.iSMS,'(a)',iostat=status) line
            if(status/=0) then
                call ErrMsg('Unexpected end of SMS file')
            end if

            if(line(1:1)=='#') then
                !write(io_tagged,'(a)') line
                cycle
            else
                exit
            end if
        end do

        if(LINMETH == 1 .and. nOpt==0) then
            write(io_tagged,'(a)') '#------------------------------------------------------------------------------------------'
            write(io_tagged,'(a)') '#For the χMD solver (Ibaraki, 2005):'
            write(io_tagged,'(a)') '#3.         IACL      NORDER     LEVEL      NORTH     IREDSYS RRCTOL        IDROPTOL  EPSRN'

            read(line,*) IACL, NORDER, LEVEL, NORTH, IREDSYS, RRCTOL, IDROPTOL, EPSRN
            write(TmpSTR,'(5x,5(i10,1x),1(1pg10.1,1x),1(i10,1x),1(1pg10.1,1x))') IACL, NORDER, LEVEL, NORTH, IREDSYS, RRCTOL, IDROPTOL, EPSRN
            write(io_tagged,'(a)') trim(TmpSTR)

            write(io_tagged,'(a)') '#IACL—is the flag for choosing the acceleration method.'
            write(io_tagged,'(a)') '#    0 – Conjugate Gradient – select this option if the matrix is symmetric.'
            write(io_tagged,'(a)') '#    1 – ORTHOMIN'
            write(io_tagged,'(a)') '#    2 - BiCGSTAB'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#NORDER—is the flag for choosing the ordering scheme.'
            write(io_tagged,'(a)') '#    0 – original ordering'
            write(io_tagged,'(a)') '#    1 – reverse Cuthill McKee ordering'
            write(io_tagged,'(a)') '#    2 – Minimum degree ordering'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#LEVEL—is the level of fill for ILU decomposition. Higher levels of fill'
            write(io_tagged,'(a)') '#    provide more robustness but also require more memory. For optimal'
            write(io_tagged,'(a)') '#    performance, it is suggested that a large level of fill be applied (7 or 8)'
            write(io_tagged,'(a)') '#    with use of drop tolerance.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#NORTH—is the number of orthogonalizations for the ORTHOMIN acceleration'
            write(io_tagged,'(a)') '#    scheme. A number between 4 and 10 is appropriate. Small values require less'
            write(io_tagged,'(a)') '#    storage but more iteration may be required. This number should equal 2 for'
            write(io_tagged,'(a)') '#    the other acceleration methods.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#IREDSYS—is the index for creating a reduced system of equations using the'
            write(io_tagged,'(a)') '#    red-black ordering scheme.'
            write(io_tagged,'(a)') '#    0 – do not create reduced system'
            write(io_tagged,'(a)') '#    1 – create reduced system using red-black ordering'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#RRCTOL—is a residual tolerance criterion for convergence. The root mean'
            write(io_tagged,'(a)') '#    squared residual of the matrix solution is evaluated against this number to'
            write(io_tagged,'(a)') '#    determine convergence. The solver assumes convergence if either HICLOSE (the'
            write(io_tagged,'(a)') '#    absolute head tolerance value for the solver) or RRCTOL is achieved. Note'
            write(io_tagged,'(a)') '#    that a value of zero ignores residual tolerance in favor of the absolute'
            write(io_tagged,'(a)') '#    tolerance (HICLOSE) for closure of the matrix solver.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#IDROPTOL—is the flag to perform drop tolerance.'
            write(io_tagged,'(a)') '#    0 – do not perform drop tolerance'
            write(io_tagged,'(a)') '#    1 – perform drop tolerance'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#EPSRN—is the drop tolerance value. A value of 10-3 works well for most problems'
            write(io_tagged,'(a)') '#'
        else if(LINMETH == 2 .and. nOpt==0) then
            write(io_tagged,'(a)') '#----------------------------------------------------------------------------------------'
            write(io_tagged,'(a)') '#For PCGU Solver (White and Hughes, 2011):'
            write(io_tagged,'(a)') '#4. CLIN IPC ISCL IORD RCLOSEPCGU RELAXPCGU'

            read(line,*) CLIN, IPC, ISCL, IORD, RCLOSEPCGU, RELAXPCGU
            write(TmpSTR,'(5x,1(1pg10.1,1x),3(i10,1x),2(1pg10.1,1x))') CLIN, IPC, ISCL, IORD, RCLOSEPCGU, RELAXPCGU
            write(io_tagged,'(a)') trim(TmpSTR)

            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#CLIN— an option keyword that defines the linear acceleration method used by the PCGU solver.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    CLIN = ‘CG’, preconditioned conjugate gradient method.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    CLIN = ‘BCGS’, preconditioned bi-conjugate gradient stabilized method.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    If CLIN is not specified the preconditioned conjugate gradient method is'
            write(io_tagged,'(a)') '#    used. The preconditioned conjugate gradient method should be used for'
            write(io_tagged,'(a)') '#    problems with a symmetric coefficient matrix. The preconditioned'
            write(io_tagged,'(a)') '#    bi-conjugate gradient stabilized method should be used for problems with'
            write(io_tagged,'(a)') '#    a non-symmetric coefficient matrix (for example, with problems using the'
            write(io_tagged,'(a)') '#    Newton-Raphson linearization scheme).'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#IPC— an integer value that defines the preconditioner.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    IPC = 0, No preconditioning.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    IPC = 1, Jacobi preconditioning.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    IPC = 2, ILU(0) preconditioning.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    IPC = 3, MILU(0) preconditioning.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    IPC=3 works best for most problems.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#ISCL - flag for choosing the matrix scaling approach used.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    0 – no matrix scaling applied'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    1 – symmetric matrix scaling using the scaling method by the POLCG'
            write(io_tagged,'(a)') '#    preconditioner in Hill (1992).'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    2 – symmetric matrix scaling using the ℓ2 norm of each row of A (DR) and'
            write(io_tagged,'(a)') '#    the ℓ2 norm of each row of DRA.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    If the ILU(0) or MILU(0) preconditioners (IPC = 2 or 3) are used and'
            write(io_tagged,'(a)') '#    matrix reordering (IORD > 0) is selected, then ISCL must be 1 or 2.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#IORD is the flag for choosing the matrix reordering approach used.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    0 – original ordering'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    1 – reverse Cuthill McKee ordering'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    2 – minimum degree ordering'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#    If reordering is used, reverse Cuthill McKee ordering has been found to'
            write(io_tagged,'(a)') '#    be a more effective reordering approach for the test problems evaluated.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#RCLOSEPCGU—a real value that defines the flow residual tolerance for'
            write(io_tagged,'(a)') '#    convergence of the PCGU linear solver. This value represents the maximum'
            write(io_tagged,'(a)') '#    allowable residual at any single node. Value is in units of length cubed per'
            write(io_tagged,'(a)') '#    time, and must be consistent with MODFLOW-USG length and time units. Usually'
            write(io_tagged,'(a)') '#    a value of 1.0×10-1 is sufficient for the flow-residual criteria when meters'
            write(io_tagged,'(a)') '#    and seconds are the defined MODFLOW-USG length and time.'
            write(io_tagged,'(a)') '#'
            write(io_tagged,'(a)') '#RELAXPCGU—a real value that defines the relaxation factor used by the MILU(0)'
            write(io_tagged,'(a)') '#    preconditioner. RELAXPCGU is unitless and should be greater than or equal to'
            write(io_tagged,'(a)') '#    0.0 and less than or equal to 1.0. RELAXPCGU values of about 1.0 are commonly'
            write(io_tagged,'(a)') '#    used, and experience suggests that convergence can be optimized in some cases'
            write(io_tagged,'(a)') '#    with RELAXPCGU values of 0.97. A RELAXPCGU value of 0.0 will result in ILU(0)'
            write(io_tagged,'(a)') '#    preconditioning. RELAXPCGU is only specified if IPC=3. If RELAXPCGU is not'
            write(io_tagged,'(a)') '#    specified and IPC=3, then a default value of 0.97 will be assigned to'
            write(io_tagged,'(a)') '#    RELAXPCGU.'
        end if


        close(Modflow.iSMS)
        close(io_tagged)

        CmdLine='del '//trim(Modflow.FNameSMS)
        CALL execute_command_line(CmdLine )
        CmdLine='rename '//trim(FName)//' '//trim(Modflow.FNameSMS)
        CALL execute_command_line(CmdLine )


        return

    end subroutine MUSG_TagSMSFile
