module fem !
    use GeneralRoutines
    implicit none

    type femesh
	    ! nodes
	    integer :: NN

	    real(dr), allocatable :: X(:)    ! x-coordinate
	    real(dr), allocatable :: Y(:)    ! y-coordinate
	    real(dr), allocatable :: Z(:)    ! z-coordinate

        ! elements 
	    integer :: NE
	    integer :: NLN
	    integer*4, allocatable :: IN(:,:) 
        
        ! fracture elements
	    integer :: NEF
	    integer :: NLNF
	    integer, allocatable :: INF(:,:) 

        ! overland elements
	    integer :: nolfe
	    integer :: OlfNLN
	    integer, allocatable :: inolf(:,:) 
        
    end type femesh
    
    contains
    
    !----------------------------------------------------------------------
    subroutine avg_elemdim(mesh, avg_dx, avg_dy, avg_dz)
        !
        !  ...Compute average element dimensions
        !
        implicit none
        
        type (femesh) mesh

        integer :: i
	    real(dr) :: delt
        real(dr) :: dx,dy,dz

        real(dr) :: avg_dx
        real(dr) :: avg_dy
        real(dr) :: avg_dz


	    ! calculate average dx, dy and dz for unstructured mesh
        avg_dx=0.0d0
        avg_dy=0.0d0
        avg_dz=0.0d0
        do i=1,mesh.ne
		    if(mesh.nln.eq.8) then
			    call elemdim(mesh,i,dx,dy,dz)
			    avg_dx=avg_dx+dx
			    avg_dy=avg_dy+dy
			    avg_dz=avg_dz+dz
		    elseif(mesh.nln.eq.6) then
			    call coefpr(mesh,i,delt)
			    avg_dx=avg_dx+sqrt(delt)
			    avg_dy=avg_dy+sqrt(delt)
			    dz=dabs(mesh.z(mesh.in(4,i))+mesh.z(mesh.in(5,i))+mesh.z(mesh.in(6,i))-mesh.z(mesh.in(1,i))-mesh.z(mesh.in(2,i))-mesh.z(mesh.in(3,i)))*third
			    avg_dz=avg_dz+dz
		    end if
	    end do

	    avg_dx=avg_dx/mesh.ne
	    avg_dy=avg_dy/mesh.ne
	    avg_dz=avg_dz/mesh.ne

    end subroutine avg_elemdim
    !----------------------------------------------------------------------
    subroutine elemdim(mesh,iel,dx,dy,dz)
        !
        !  ...Compute elemental dimensions
        !
        implicit none

        type (femesh) mesh

        integer(di) :: iel
        real(dr) :: dx,dy,dz !,dlac
        !real(dr) :: el12,el34,el56,el78
        !real(dr) :: el14,el23,el58,el67
        !real(dr) :: el15,el26,el37,el48


        !if(kgrid.eq.1)then
            dx=mesh.x(mesh.in(2,iel))-mesh.x(mesh.in(1,iel))
            dy=mesh.y(mesh.in(4,iel))-mesh.y(mesh.in(1,iel))
            dz=mesh.z(mesh.in(8,iel))-mesh.z(mesh.in(4,iel))
        !elseif(kgrid.eq.0)then
        !    el12=dlac(mesh.in(2,iel),mesh.in(1,iel))
        !    el34=dlac(mesh.in(3,iel),mesh.in(4,iel))
        !    el56=dlac(mesh.in(5,iel),mesh.in(6,iel))
        !    el78=dlac(mesh.in(7,iel),mesh.in(8,iel))
        !    dx=0.25d0*(el12+el34+el56+el78)
        !    el14=dlac(mesh.in(1,iel),mesh.in(4,iel))
        !    el23=dlac(mesh.in(2,iel),mesh.in(3,iel))
        !    el58=dlac(mesh.in(5,iel),mesh.in(8,iel))
        !    el67=dlac(mesh.in(6,iel),mesh.in(7,iel))
        !    dy=0.25d0*(el14+el23+el58+el67)
        !    el15=dlac(mesh.in(1,iel),mesh.in(5,iel))
        !    el26=dlac(mesh.in(2,iel),mesh.in(6,iel))
        !    el37=dlac(mesh.in(3,iel),mesh.in(7,iel))
        !    el48=dlac(mesh.in(4,iel),mesh.in(8,iel))
        !    dz=0.25d0*(el15+el26+el37+el48)
        !end if

        return
    end subroutine elemdim
    !----------------------------------------------------------------------
    function dlac(mesh,n1,n2)
        !
        !  ...Finds the absolute distance between 2 nodes
        !
        implicit none

        type (femesh) mesh

        integer(di) :: n1,n2
        real(dr) :: term,dlac
	    real(dr) :: term1,term2,term3

    !    term=(x(n1)-x(n2))**2 + (y(n1)-y(n2))**2 + (z(n1)-z(n2))**2
        term1=mesh.x(n1)-mesh.x(n2)
        term2=mesh.y(n1)-mesh.y(n2)
        term3=mesh.z(n1)-mesh.z(n2)
	    term=term1*term1 + term2*term2 + term3*term3
        dlac=dsqrt(term)
        return
    end function dlac
    !tg-nov02 - added following function
    !----------------------------------------------------------------------
    subroutine coefpr(mesh,iel,delt)
        !
        !  ...Compute the area for prism elements
        !
        implicit none

        type (femesh) mesh

        integer(di) :: iel
        real(dr) :: x2,x3,y2,y3
        real(dr) :: delt

        x2 = mesh.x(mesh.in(2,iel)) - mesh.x(mesh.in(1,iel))
        x3 = mesh.x(mesh.in(3,iel)) - mesh.x(mesh.in(1,iel))
        y2 = mesh.y(mesh.in(2,iel)) - mesh.y(mesh.in(1,iel))
        y3 = mesh.y(mesh.in(3,iel)) - mesh.y(mesh.in(1,iel))
        delt = dabs(x2*y3 - x3*y2)*0.5d0
    end subroutine coefpr
    
    !----------------------------------------------------------------------
    subroutine find_elem_centroid(mesh,ie,xc,yc,zc)
        implicit none

        type (femesh) mesh

	    integer :: i
        integer :: ie
        real(dr) :: xsum, ysum, zsum
	    real(dr) :: xc, yc, zc

        xsum=0.0
        ysum=0.0
        zsum=0.0
        do i=1,mesh.nln
            xsum=xsum+mesh.x(mesh.in(i,ie))
            ysum=ysum+mesh.y(mesh.in(i,ie))
            zsum=zsum+mesh.z(mesh.in(i,ie))
        end do
        xc=xsum/mesh.nln
        yc=ysum/mesh.nln
        zc=zsum/mesh.nln

    end subroutine find_elem_centroid
    
    !----------------------------------------------------------------------
    subroutine InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
        implicit none

        real*8, intent(in) :: x(3),y(3)
        real*8, intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)

        integer :: i,j
        integer :: npt=3
        real*8 :: s,x1,x2,y1,y2
        real*8 :: small = 1.0d-10
        real*8 :: PI

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

        real*8, intent(in) :: x(3),y(3)
        real*8, intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)
        logical, intent(out) :: bad_triangle

        integer :: i,j,k
        integer :: npt=3
        real*8 :: x1,x2,y1,y2,a11,a12,a21,a22,r1,r2,det,xmid,ymid,side(3)
        real*8 :: small = 1.0d-10
        real*8 :: PI

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


end module fem 
