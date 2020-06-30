module gmap
    use enemies
    implicit none

    type Room
        integer :: i, j ! Room Position - relative
        integer :: ox, oy ! Room Offset
        integer :: x, y
        integer :: width, height
    end type Room
contains
    ! This is a horrible map generator, not very efficient.
    subroutine generate_map(game_map, player_x, player_y)
        implicit none
        type(Room), dimension(:), allocatable :: room_list
        integer, dimension(15) :: rand_rooms
        character, dimension(:,:), allocatable, intent(inout) :: game_map
        integer, dimension(:,:), allocatable :: visited
        integer, intent(inout) :: player_x, player_y
        integer :: width, height ! map w & h
        integer :: d_w, d_h
        integer :: xi, yi, i, j, rmnum ! iterators
        integer :: central, g_x, g_y, c_x, c_y ! hallway generation
        type(Room) :: c_room, g_room
        integer :: wall
        logical :: h_priority, already_visited

        ! Get map w & height
        width = SIZE(game_map, 1)
        height = SIZE(game_map, 2)

        ! allocate room for room_list & visited tiles
        allocate(room_list(width*height))
        allocate(visited(width,height))


        ! get "dimensions" -- how many rooms there are going to be
        d_w = width / 5
        d_h = height / 3

        ! Fill map with concrete & set unvisited
        do xi=1,width
            do yi=1,height
                game_map(xi, yi) = '#'
                visited(xi, yi) = 0
            end do
        end do

        ! Generate the rooms & add them to the room list
        rmnum = 1
        do i=1,5
            do j=1,3
                c_room%i = i
                c_room%j = j
                c_room%width = 3 + (RAND() * (d_w - 3))
                c_room%height = 3 + (RAND() * (d_h - 3))
                c_room%ox = RAND() * (d_w - c_room%width)
                c_room%oy = RAND() * (d_h - c_room%height)
                c_room%x = ((i - 1) * d_w + c_room%ox)
                c_room%y = ((j - 1) * d_h + c_room%oy)

                do xi=1,c_room%width
                    do yi=1,c_room%height
                        game_map(c_room%x + xi, c_room%y +  yi) = '.'
                    end do
                end do
                room_list(rmnum) = c_room
                rmnum = rmnum + 1
            end do
        end do

        ! Create Hallways !

        ! All roads lead to this room
        central = 1 + RAND() * 15
        g_room = room_list(central)
        do xi=1, g_room%width
            do yi=1,g_room%height
                visited(g_room%x + xi, g_room%y +  yi) = 1
            end do
        end do

        ! Shuffle list of rooms
        rand_rooms = scramble(15)

        do i=1,15
            if (rand_rooms(i) .ne. central) then

                h_priority = RAND() .lt. 0.5 ! Will the hallway move horizontally first or vertically?
                already_visited = .false.

                c_room = room_list(rand_rooms(i)) ! get the current room

                ! check to make sure this room hasn't already been passed through
                do xi=1, c_room%width
                    do yi=1,c_room%height
                        if (visited(c_room%x + xi, c_room%y +  yi) .eq. 1) then
                            already_visited = .true.
                            exit
                        end if
                    end do
                    if (already_visited) then
                        exit
                    end if
                end do

                ! choose a random point in start room & target room to connect
                c_x = c_room%x + (1 + RAND() * c_room%width)
                c_y = c_room%y + (1 + RAND() * c_room%height)
                g_x = g_room%x + (1 + RAND() * g_room%width)
                g_y = g_room%y + (1 + RAND() * g_room%height)

                ! connect the dots
                do while (((c_x .ne. g_x) .or. (c_y .ne. g_y)) .and. (visited(c_x, c_y) .ne. 1) .and. .not. already_visited)
                    game_map(c_x, c_y) = '.'
                    visited(c_x, c_y) = 1

                    if (h_priority) then
                        if (c_x .lt. g_x) then
                            c_x = c_x + 1
                        else if (c_x .gt. g_x) then
                            c_x = c_x - 1
                        else if (c_y .lt. g_y) then
                            c_y = c_y + 1
                        else if (c_y .gt. g_y) then
                            c_y = c_y - 1
                        end if
                    else
                        if (c_y .lt. g_y) then
                            c_y = c_y + 1
                        else if (c_y .gt. g_y) then
                            c_y = c_y - 1
                        else if (c_x .lt. g_x) then
                            c_x = c_x + 1
                        else if (c_x .gt. g_x) then
                            c_x = c_x - 1
                        end if
                    end if
                end do

                ! fill in the room as visited
                do xi=1, c_room%width
                    do yi=1,c_room%height
                        visited(c_room%x + xi, c_room%y +  yi) = 1
                    end do
                end do
            end if
        end do

        ! Put the player in a random location
        c_room = room_list(INT(1 + RAND() * 15))
        player_x = c_room%x + (1 + RAND() * c_room%width)
        player_y = c_room%y + (1 + RAND() * c_room%height)

        ! Put Exit in a random room
        c_room = room_list(INT(1 + RAND() * 15))
        c_x = c_room%x + (1 + RAND() * c_room%width)
        c_y = c_room%y + (1 + RAND() * c_room%height)
        game_map(c_x, c_y) = '>'

        ! Place a few enemies
        if (ALLOCATED(e_list)) then
            call destroy_enemies()
        end if
        allocate(e_list(10))
        do i=1,10
            c_room = room_list(INT(1 + RAND() * 15))
            c_x = c_room%x + (1 + RAND() * c_room%width)
            c_y = c_room%y + (1 + RAND() * c_room%height)
            select case (INT(RAND() * 3))
                case (0)
                    call add_enemy(make_zagger(c_x,c_y), i)
                case (1)
                    call add_enemy(make_ambler(c_x,c_y), i)
                case (2)
                    call add_enemy(make_grognork(c_x,c_y), i)
            end select

        end do

        ! I'm the trash man. It's my character!
        deallocate(room_list)
        deallocate(visited)
    end subroutine

    function scramble( number_of_values ) result(array)
        integer, intent(in) :: number_of_values
        integer, dimension(:), allocatable   :: array
        integer :: i, j, k, m, n
        integer :: temp
        real :: u

        array=[(i,i=1,number_of_values)]

        n=1
        m=number_of_values
        do k=1,2
            do i=1,m
                call random_number(u)
                j = n + FLOOR((m+1-n)*u)
                temp=array(j)
                array(j)=array(i)
                array(i)=temp
            end do
        end do

    end function scramble

end module gmap
