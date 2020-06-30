module map_mod
    character, dimension(:,:), allocatable :: game_map
    integer :: map_w, map_h
contains
    ! Collision Check
    logical function check_coll(x, y) result(can_pass)
        implicit none
        integer, value :: x, y
        if (x .lt. 1 .or. x .gt. map_w .or. y .lt. 1 .or. y .gt. map_h) then
            can_pass = .false.
        else
            can_pass = .not. (game_map(x,y) .eq. '#')
        end if
    end function
end module map_mod
