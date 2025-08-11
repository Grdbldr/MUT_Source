module NumericalMesh
    use BasicTypes
    use GeneralRoutines
    implicit none
    
    type node 
        real(dp) :: x
        real(dp) :: y
        real(dp) :: z
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
    end type node 
    
    type element
        character(40) :: Typ ! element typ eg triangle, quadrilateral, prism, block etc
        !character(len=:), allocatable :: Typ ! number of nodes in element
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
        integer(i4) :: nZones
        integer(i4) :: idZone
    end type element    

    type mesh
        character(len=:), allocatable :: name
        integer(i4) :: id
        character(128) :: ElementType      ! eg triangle, quadrilateral, prism, block etc
        character(MAX_LBL) :: STR_LengthUnit

        integer(i4) :: nNodes              ! number of nodes in the femesh
        type(node), allocatable :: node(:) ! array of nodes
       
        integer(i4) :: nElements              ! number of elements in the femesh
        type(element), allocatable :: element(:) ! array of elements

        integer(i4) :: nNodesPerElement ! number of nodes in element
        integer(i4), allocatable :: idNode(:,:) ! array of local node ids for elements

    end type mesh

    contains
    
    !----------------------------------------------------------------------
    subroutine InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
        implicit none

        real(dp), intent(in) :: x(3),y(3)
        real(dp), intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)

        integer(i4) :: i,j
        integer(i4) :: npt=3
        real(dp) :: s,x1,x2,y1,y2
        real(dp) :: small = 1.0d-10
        real(dp) :: PI

        PI = 4.0d0*atan(1.0)

        x1=x(2)-x(1); y1=y(2)-y(1)
        x2=x(3)-x(1); y2=y(3)-y(1)
        area=0.5d0*dabs(x1*y2-x2*y1)

        lseg=0.0d0
        aseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                lseg(i,j)=dsqrt((x(i)-x(j))**2.0d0+(y(i)-y(j))**2.0d0)
                lseg(j,i)=lseg(i,j)
                if(dabs(x(i)-x(j))<small) then
                    aseg(i,j)=0.5d0*PI
                    aseg(j,i)=0.5d0*PI
                elseif(x(i)>x(j)) then
                    aseg(i,j)=atan((y(i)-y(j))/(x(i)-x(j)))
                    aseg(j,i)=aseg(i,j)
                else
                    aseg(i,j)=atan((y(j)-y(i))/(x(j)-x(i)))
                    aseg(j,i)=aseg(i,j)
                end if
            end do
        end do

        s=0.5d0*(lseg(1,2)+lseg(2,3)+lseg(3,1))
        xc=0.5d0*(lseg(1,2)*x(3)+lseg(2,3)*x(1)+lseg(3,1)*x(2))/s
        yc=0.5d0*(lseg(1,2)*y(3)+lseg(2,3)*y(1)+lseg(3,1)*y(2))/s
        radius=dsqrt((s-lseg(1,2))*(s-lseg(2,3))*(s-lseg(3,1))/s)

        dseg(:,:)=radius
        dseg(1,1)=0.0d0; dseg(2,2)=0.0d0; dseg(3,3)=0.0d0

        return
    end subroutine InnerCircle

    !----------------------------------------------------------------------
    subroutine OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
        implicit none

        real(dp), intent(in) :: x(3),y(3)
        real(dp), intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)
        logical, intent(out) :: bad_triangle

        integer(i4) :: i,j,k
        integer(i4) :: npt=3
        real(dp) :: x1,x2,y1,y2,a11,a12,a21,a22,r1,r2,det,xmid,ymid,side(3)
        real(dp) :: small = 1.0d-10
        real(dp) :: PI

        PI = 4.0d0*atan(1.0)

        x1=x(2)-x(1); y1=y(2)-y(1)
        x2=x(3)-x(1); y2=y(3)-y(1)
        area=0.5d0*dabs(x1*y2-x2*y1)

        lseg=0.0d0
        aseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                lseg(i,j)=dsqrt((x(i)-x(j))**2.0d0+(y(i)-y(j))**2.0d0)
                lseg(j,i)=lseg(i,j)
                if(dabs(x(i)-x(j))<small) then
                    aseg(i,j)=0.5d0*PI
                    aseg(j,i)=0.5d0*PI
                elseif(x(i)>x(j)) then
                    aseg(i,j)=atan((y(i)-y(j))/(x(i)-x(j)))
                    aseg(j,i)=aseg(i,j)
                else
                    aseg(i,j)=atan((y(j)-y(i))/(x(j)-x(i)))
                    aseg(j,i)=aseg(i,j)
                end if
            end do
        end do

        side(1)=lseg(2,3); side(2)=lseg(3,1); side(3)=lseg(1,2)
        if(side(1)>=max(side(2),side(3))) then
            k=1
        elseif(side(2)>=side(3)) then
            k=2
        else
            k=3
        end if
        i=k+1; if(i>3) i=i-3
        j=k+2; if(j>3) j=j-3
        bad_triangle=.false.
        if(side(k)**2.0d0>1.0001d0*(side(i)**2.0d0+side(j)**2.0d0)) bad_triangle=.true.

        a11=2.0d0*(x(1)-x(2)); a12=2.0d0*(y(1)-y(2)); a21=2.0d0*(x(1)-x(3)); a22=2.0d0*(y(1)-y(3))
        r1=x(1)*x(1)-x(2)*x(2)+y(1)*y(1)-y(2)*y(2); r2=x(1)*x(1)-x(3)*x(3)+y(1)*y(1)-y(3)*y(3)
        det=a11*a22-a12*a21
        if(dabs(det)<small) then
            stop 'bad triangle'
        end if
        xc=1.0d0/det*(a22*r1-a12*r2)
        yc=1.0d0/det*(-a21*r1+a11*r2)

        radius=dsqrt((xc-x(1))**2.0d0+(yc-y(1))**2.0d0)
        radius=dsqrt((xc-x(2))**2.0d0+(yc-y(2))**2.0d0)
        radius=dsqrt((xc-x(3))**2.0d0+(yc-y(3))**2.0d0)

        dseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                xmid=0.5d0*(x(i)+x(j)); ymid=0.5d0*(y(i)+y(j))
                dseg(i,j)=dsqrt((xc-xmid)**2.0d0+(yc-ymid)**2.0d0)
                dseg(j,i)=dseg(i,j)
            end do
        end do

        return
    end subroutine OuterCircle


end module NumericalMesh 
