program hanoi
    implicit none
    integer :: n
    
    n = -1
    print *, "Input number of disks"
    read(*, *) n
    if (n < 1) then
        stop "Input a positive integer"
    end if

    call solve(n, 1, 3, 2)

    contains 

    recursive subroutine solve(n, src, dst, tmp)
        integer, intent(in) :: n, src, dst, tmp
        if (n == 1) then
            call printstatus(n, src, dst)
            return
        end if
        call solve(n - 1, src, tmp, dst)
        call printstatus(n, src, dst)
        call solve(n - 1, tmp, dst, src)
    end subroutine solve

    subroutine printstatus(n, src, dst)
        integer, intent(in) :: n, src, dst
        write(*, "(A, I2, A, I2, A, I2)") "Disk ", n, ": ", src, " => ", dst
    end subroutine printstatus

end program hanoi