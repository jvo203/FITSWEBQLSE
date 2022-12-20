module contour
    ! implicit none
    use list

    private
    public :: conrec

contains
    !======================================================================
    !
    !     CONREC is a contouring subroutine for rectangularily spaced data.
    !
    !     It emits calls to a line drawing subroutine supplied by the user
    !     which draws a contour map corresponding to real*4data on a randomly
    !     spaced rectangular grid. The coordinates emitted are in the same
    !     units given in the x() and y() arrays.

    !     Any number of contour levels may be specified but they must be
    !     in order of increasing value.

    !     subroutine conrec(d,ilb,iub,jlb,jub,x,y,nc,z,contours,lines)
    !     real*4 d(ilb:iub,jlb:jub)  ! matrix of data to contour
    !     integer ilb,iub,jlb,jub    ! index bounds of data matrix
    !     real*4 x(ilb:iub)          ! data matrix column coordinates
    !     real*4 y(jlb,jub)          ! data matrix row coordinates
    !     integer nc                 ! number of contour levels
    !     real*4 z(1:nc)             ! contour levels in increasing order
    !     real*4 contours(1:nc,1:4)  ! output list of contour coordinates
    !     real*4 contours(1:nc,1:4)  ! output vector of contour coordinates

    function conrec(d, ilb, iub, jlb, jub, x, y, nc, z, contours, lines) result(line_count)
        real(kind=4) d(ilb:iub, jlb:jub)
        integer ilb, iub, jlb, jub
        real(kind=4) x(ilb:iub)
        real(kind=4) y(jlb:jub)
        integer nc
        real(kind=4) z(1:nc)
        type(list_t), pointer :: contours
        integer :: line_count, lines(:, :)
        !
        !     Local declarations
        !
        real(kind=4) h(0:4)
        integer sh(0:4)
        real(kind=4) xh(0:4), yh(0:4)
        integer im(1:4), jm(1:4)
        integer case
        integer castab(-1:1, -1:1, -1:1)
        integer p1, p2
        !
        !     Data
        !
        data im/0, 1, 1, 0/
        data jm/0, 0, 1, 1/
        data castab/0, 0, 9, 0, 1, 5, 7, 4, 8,&
        &0, 3, 6, 2, 3, 2, 6, 3, 0,&
        &8, 4, 7, 5, 1, 0, 9, 0, 0/
        !
        !     Use statement functions for the line intersections
        !
        xsect(p1, p2) = (h(p2)*xh(p1) - h(p1)*xh(p2))/(h(p2) - h(p1))
        ysect(p1, p2) = (h(p2)*yh(p1) - h(p1)*yh(p2))/(h(p2) - h(p1))
        !
        !     Scan the arrays, top down, left to right within rows
        !

        line_count = 0

20      do 100 j = jub - 1, jlb, -1
            do 90 i = ilb, iub - 1
                dmin = min(d(i, j), d(i, j + 1), d(i + 1, j), d(i + 1, j + 1))
                dmax = max(d(i, j), d(i, j + 1), d(i + 1, j), d(i + 1, j + 1))
                if (dmax .ge. z(1) .and. dmin .le. z(nc)) then
                    do 80 k = 1, nc
                        if (z(k) .ge. dmin .and. z(k) .le. dmax) then
                            do 22 m = 4, 0, -1
                                if (m .gt. 0) then
                                    h(m) = d(i + im(m), j + jm(m)) - z(k)
                                    xh(m) = x(i + im(m))
                                    yh(m) = y(j + jm(m))
                                else
                                    h(0) = 0.25*(h(1) + h(2) + h(3) + h(4))
                                    xh(0) = 0.5*(x(i) + x(i + 1))
                                    yh(0) = 0.5*(y(j) + y(j + 1))
                                end if
                                if (h(m) .gt. 0.0) then
                                    sh(m) = +1
                                else if (h(m) .lt. 0.0) then
                                    sh(m) = -1
                                else
                                    sh(m) = 0
                                end if
22                              continue
                                !
                                !     Note: at this stage the relative heights of the corners and the
                                !     centre are in the h array, and the corresponding coordinates are
                                !     in the xh and yh arrays. The centre of the box is indexed by 0
                                !     and the 4 corners by 1 to 4 as shown below.
                                !     Each triangle is then indexed by the parameter m, and the 3
                                !     vertices of each triangle are indexed by parameters m1,m2,and m3.
                                !     It is assumed that the centre of the box is always vertex 2 though
                                !     this isimportant only when all 3 vertices lie exactly on the same
                                !     contour level, in which case only the side of the box is drawn.
                                !
                                !
                                !           vertex 4 +-------------------+ vertex 3
                                !                    | \               / |
                                !                    |   \    m-3    /   |
                                !                    |     \       /     |
                                !                    |       \   /       |
                                !                    |  m=2    X   m=2   |       the centre is vertex 0
                                !                    |       /   \       |
                                !                    |     /       \     |
                                !                    |   /    m=1    \   |
                                !                    | /               \ |
                                !           vertex 1 +-------------------+ vertex 2
                                !
                                !
                                !
                                !                    Scan each triangle in the box
                                !
                                do 60 m = 1, 4
                                    m1 = m
                                    m2 = 0
                                    if (m .ne. 4) then
                                        m3 = m + 1
                                    else
                                        m3 = 1
                                    end if
                                    case = castab(sh(m1), sh(m2), sh(m3))
                                    if (case .ne. 0) then
                                        goto(31, 32, 33, 34, 35, 36, 37, 38, 39), case
                                        !
                                        !     Case 1 - Line between vertices 1 and 2
                                        !
31                                      x1 = xh(m1)
                                        y1 = yh(m1)
                                        x2 = xh(m2)
                                        y2 = yh(m2)
                                        goto 40
                                        !
                                        !     Case 2 - Line between vertices 2 and 3
                                        !
32                                      x1 = xh(m2)
                                        y1 = yh(m2)
                                        x2 = xh(m3)
                                        y2 = yh(m3)
                                        goto 40
                                        !
                                        !     Case 3 - Line between vertices 3 and 1
                                        !
33                                      x1 = xh(m3)
                                        y1 = yh(m3)
                                        x2 = xh(m1)
                                        y2 = yh(m1)
                                        goto 40
                                        !
                                        !     Case 4 - Line between vertex 1 and side 2-3
                                        !
34                                      x1 = xh(m1)
                                        y1 = yh(m1)
                                        x2 = xsect(m2, m3)
                                        y2 = ysect(m2, m3)
                                        goto 40
                                        !
                                        !     Case 5 - Line between vertex 2 and side 3-1
                                        !
35                                      x1 = xh(m2)
                                        y1 = yh(m2)
                                        x2 = xsect(m3, m1)
                                        y2 = ysect(m3, m1)
                                        goto 40
                                        !
                                        !     Case 6 - Line between vertex 3 and side 1-2
                                        !
36                                      x1 = xh(m3)
                                        y1 = yh(m3)
                                        x2 = xsect(m1, m2)
                                        y2 = ysect(m1, m2)
                                        goto 40
                                        !
                                        !     Case 7 - Line between sides 1-2 and 2-3
                                        !
37                                      x1 = xsect(m1, m2)
                                        y1 = ysect(m1, m2)
                                        x2 = xsect(m2, m3)
                                        y2 = ysect(m2, m3)
                                        goto 40
                                        !
                                        !     Case 8 - Line between sides 2-3 and 3-1
                                        !
38                                      x1 = xsect(m2, m3)
                                        y1 = ysect(m2, m3)
                                        x2 = xsect(m3, m1)
                                        y2 = ysect(m3, m1)
                                        goto 40
                                        !
                                        !     Case 9 - Line between sides 3-1 and 1-2
                                        !
39                                      x1 = xsect(m3, m1)
                                        y1 = ysect(m3, m1)
                                        x2 = xsect(m1, m2)
                                        y2 = ysect(m1, m2)
                                        goto 40
40                                      line_count = line_count + 1
                                        ! lines(1:5, line_count) = (/nint(x1), nint(y1), nint(x2), nint(y2), k/)
                                        call vecout(x1, y1, x2, y2, k, contours) ! was z(k)
                                    end if
60                                  continue
                                    end if
80                                  continue
                                    end if
90                                  continue
100                                 continue
                                    return
                                    end function conrec

                                    subroutine vecout(x1, y1, x2, y2, z, contours)
                                        implicit none
                                        real(kind=4) x1, y1, x2, y2
                                        integer z
                                        type(list_t), pointer :: contours
                                        integer :: pos(5)
                                        !
                                        !***** Replace from here
                                        !
                                        !     The following should be ignored since it is specific to
                                        !     the version of FORTRAN running on the Macintosh microcomputer
                                        !     on which this particular example was written.
                                        !
                                        ! INTEGER LINETO
                                        ! PARAMETER (LINETO=Z'89109000')
                                        ! INTEGER MOVETO
                                        ! PARAMETER (MOVETO=Z'89309000')
                                        ! call toolbx(MOVETO,nint(x1),nint(y1))
                                        ! call toolbx(LINETO,nint(x2),nint(y2))

                                        ! print *, 'x1,y1,x2,y2,z = ', x1,y1,x2,y2,z

                                        ! fill-in the contour vector
                                        ! pos(1) = nint(x1)
                                        ! pos(2) = nint(y1)
                                        ! pos(3) = nint(x2)
                                        ! pos(4) = nint(y2)
                                        ! pos(5) = z

                                        ! append the line to the list
                                        ! call list_insert(contours, pos)
                                        !
                                        !***** To here
                                        !
                                        return
                                    end subroutine vecout
                                    end module contour
