    program USG2SWF

    implicit none

    integer :: i,j,i1,i2
    
    integer :: nCell2D,nLayer,nCell,nJa
    real*8, allocatable :: Top(:),Bot(:),Area(:),Clength(:),PArea(:)
    integer, allocatable :: ia(:),ja(:),iain(:),ibound(:)
    
    integer :: nJa2D,icount
    integer, allocatable :: ia2Din(:),ja2D(:),ia2D(:)
    real*8, allocatable :: PA2D(:),CL2D(:),iHead(:)
    integer :: iftyp,isswadi,ifcon,isgwadi
    real*8 :: sgcl,A1,A2,A12
    
    nCell2D = 1610
    nLayer = 9
    nCell = 14490
    nJa = 96536
    
    allocate(Top(nCell2D),Bot(nCell2D),Area(nCell2D),iain(nCell),ia(nCell+1),ja(nJa),Clength(nJa),PArea(nJa),ibound(nCell2D),iHead(nCell2D))
    
    open(1004,file='IAJAGWF.dat')
    read(1004,*)
    read(1004,*) (Top(i),i=1,nCell2D)
    read(1004,*)
    read(1004,*) (Bot(i),i=1,nCell2D)
    read(1004,*)
    read(1004,*) (Area(i),i=1,nCell2D)
    read(1004,*)
    read(1004,*) (iain(i),i=1,nCell)
    read(1004,*)
    read(1004,*) (ja(i),i=1,nJa)
    read(1004,*)
    read(1004,*) (CLength(i),i=1,nJa)
    read(1004,*)
    read(1004,*) (PArea(i),i=1,nJa)
    read(1004,*)
    read(1004,*) (ibound(i),i=1,nCell2D)
    close(1004)
    
    ia(1)=1
    do i=1,nCell
        ia(i+1)=ia(i)+iain(i)
    enddo
    
    allocate(ia2Din(nCell),ia2D(nCell2D+1))
    nJa2D=0
    do i=1,nCell2D
        ia2Din(i)=iain(i)-1
        nJa2D=nJa2D+ia2Din(i)
    enddo
    ia2D(1)=1
    do i=1,nCell2D
        ia2D(i+1)=ia2D(i)+ia2Din(i)
    enddo
    
    allocate(ja2D(nJa2D),CL2D(nJa2D),PA2d(nJa2D))
    icount=0
    do i=1,nCell2D
        i1=ia(i)
        i2=ia(i+1)-1
        do j=i1,i2
            if(ja(j)<=nCell2D) then
                icount=icount+1
                ja2D(icount)=ja(j)
                CL2D(icount)=CLength(j)
                PA2D(icount)=PArea(j)
            endif
        enddo
    enddo
    if(icount/=nJa2D) stop 'check nJa2D'
    
    do i=1,nCell2D
        i1=ia2D(i)
        i2=ia2D(i+1)-1
        do j=i1,i2
            if(ja2D(j)==ja2D(i1)) then
                PA2D(j)=0.0d0
            else
                A1=Top(ja2D(i1))-Bot(ja2D(i1))
                A2=Top(ja2D(j))-Bot(ja2D(j))
                A12=0.5d0*(A1+A2)
                PA2D(j)=PA2D(j)/A12
            endif
        enddo
    enddo
    
    
    open(1004,file='abdul1.swf')
    
    write(1004,'(a)') '#1. NSWFNDS  NJA_SWF  NSWFGWC   NSWFTYP  ISWFCB  ISWFHD   ISWFDD    ISWFIB'
    write(1004,'(a)') '     1610     7864     1610        1      118      119      120        0'
    write(1004,'(a)') 'INTERNAL  1  (FREE)  -1  IA()'
    write(1004,'(10i4)') (ia2Din(i),i=1,nCell2D)
    write(1004,'(a)') 'INTERNAL  1  (FREE)  -1  JA()'
    do i=1,nCell2D
        i1=ia2D(i)
        i2=ia2D(i+1)-1
        write(1004,'(10i10)') (ja2D(j),j=i1,i2)
    end do
    
    write(1004,'(a)') '# IFNO IFTYP   FAREA          FELEV      ISSWADI'
    iftyp=1; isswadi=0
    do i=1,nCell2D
        write(1004,'(i10,i5,2(1pG15.5),i5)') i,iftyp,Area(i),Top(i),isswadi
    enddo
    
    write(1004,'(a)') '# IFNO IFGWNO  IFCON     SGCL        SGCAREA      ISGWADI'
    ifcon=1; sgcl=0.001d0; isgwadi=0
    do i=1,nCell2D
        write(1004,'(2i10,3x,i5,2(1pG15.5),i5)') i,i,ifcon,sgcl,Area(i),isgwadi
    enddo
    
    write(1004,'(a)') '# ISWFTYP      SMANN          SWFH1          SWFH2'
    write(1004,'(a)') '    1       3.00000E-02    1.00000E-06    1.00000E-06'
    
    write(1004,'(a)') 'INTERNAL  1  (FREE)  -1  Connection Length CLN()'
    do i=1,nCell2D
        i1=ia2D(i)
        i2=ia2D(i+1)-1
        write(1004,'(10(1pg15.8))') (CL2D(j),j=i1,i2)
    end do
    
    write(1004,'(a)') 'INTERNAL  1  (FREE)  -1  Perpendicular Area FAHL()'
    do i=1,nCell2D
        i1=ia2D(i)
        i2=ia2D(i+1)-1
        write(1004,'(10(1pg15.8))') (PA2D(j),j=i1,i2)
    end do
    
    write(1004,'(a)') 'INTERNAL  1  (FREE)  -1  IBOUND'
    write(1004,'(10i3)') (ibound(i),i=1,nCell2D)
    
    do i=1,nCell2D
        iHead(i)=Top(i)+1.0d-6
    enddo
    
    write(1004,'(a)') 'INTERNAL  1.000000e+000  (FREE)  -1  Starting Heads()'
    write(1004,'(5(1pg15.8))') (IHead(i),i=1,nCell2D)
    close(1004)
    
    end program USG2SWF

