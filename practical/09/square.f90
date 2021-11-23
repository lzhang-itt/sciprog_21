include "magic_square.fh"

program square
    use msquare

    implicit none

    integer :: i, n, readerr
    integer, dimension(:, :), allocatable :: s
    ! haracter(16), parameter :: filename = "square_magic.csv"
    character(10), parameter :: filename = "square.csv"
    integer, parameter :: n_default = 4 ! 3
    
    n = n_default

    open(unit=1, file=filename, form="formatted", access="sequential", action="read", status="old", iostat=readerr)
    if (readerr /= 0) then
        stop "Error opening file"
    end if

    allocate(s(n,n))

    ! read in every row
    do, i = 1, n
        read(1, *, iostat=readerr) s(i, :)
    end do

    if (readerr /= 0) then
        stop "File format error"
    end if

    close(unit=1, status="keep")
    
    if (isMagicSquare(s, n)) then
        print *, "TRUE"
    else 
        print *, "FALSE"
    end if

    deallocate(s)
    ! time complexity here should be O(n^2)
    ! best case could be O(n)
    
end program square