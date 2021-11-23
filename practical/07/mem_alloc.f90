program alloc
    integer, dimension(6), parameter :: arraysizes = (/0, 1, 10, 100, 1000, 10000/) ! could fail on low ram devices if two more digits
    integer :: k
    integer, dimension(:), pointer :: p
    
    ! not too much fun in fortran with pointers. Which is a good thing.
    do, k = 1, size(arraysizes)
        write(*, "(A14,I0)") "Array length: ", arraysizes(k)
        p => allocatearray(arraysizes(k))
        call fillwithones(p)
        call printarray(p)
        call deallocatearray(p)
    end do

    contains

    function allocatearray(n) result(arrptr)
        integer, dimension(:), pointer :: arrptr
        integer, intent(in) :: n
        integer :: allocstat
        ! if (associated(arrptr)) then
        !     stop "Attempting to overwrite pointer"
        ! end if
        allocate(arrptr(n), stat=allocstat)
        if (allocstat /= 0) then
            stop "Unable to allocate memory."
        end if
    end function allocatearray

    subroutine fillwithones(arrptr)
        integer, dimension(:), pointer, intent(inout) :: arrptr
        arrptr = 1
    end subroutine fillwithones

    subroutine printarray(arrptr)
        integer, dimension(:), pointer, intent(inout) :: arrptr
        integer :: i
        write(*, "(100I2)") (arrptr(i), i=1, size(arrptr)) 
    end subroutine printarray

    subroutine deallocatearray(arrptr)
        integer, dimension(:), pointer, intent(inout) :: arrptr
        if (associated(arrptr)) then
            deallocate(arrptr)
        end if
    end subroutine deallocatearray

end program alloc