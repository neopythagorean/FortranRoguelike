program rogue_like
    use iso_c_binding
    use bearlibterminal
    use gmap
    use map_mod
    use enemies
    use mplayer
    implicit none

    integer :: user_in, level = 0
    logical :: quit = .false.

    character(len=5), parameter :: sword_art = "<==|-"

    ! Iterators
    integer :: xi, yi, i

    ! Format String
    character(len=20) :: fmstr

! Start Code

    ! Seed Random
    call SRAND(TIME())

    ! Generate Map
    allocate(game_map(70,24))
    map_w = SIZE(game_map, 1)
    map_h = SIZE(game_map, 2)
    call generate_map(game_map, p%x, p%y)

    ! Start Terminal
    call terminal_open()
    call terminal_set("window.title='Fortran Roguelike'"//CHAR(0))

    ! MAIN LOOP !
    do while (.not. quit)
        !DRAW!
        call terminal_clear()

        ! Death Message
        if (p%hp .le. 0) then
            call terminal_print(0, 0, "[color=red]YOU HAVE DIED")
            call terminal_refresh()
            user_in = terminal_read()
            exit
        end if

        ! Hud
        call terminal_print(72, 0, "[color=red]HEALTH: ")

        call terminal_color(INT8(X'FFFF0000'))
        do i=1,p%hp
            call terminal_put(79-i, 1, ICHAR('*'))
        end do
        call terminal_color(INT8(X'FFFFFFFF'))

        call terminal_print(72, 3, "[color=grey]SWORD: ")
        do i=1,p%sword
            call terminal_put(79-i, 4, ICHAR(sword_art(6-i:6-i)))
        end do

        write(fmstr,"(A7,I7)")"LEVEL:"//CHAR(10), level
        call terminal_print(72, 6, "[color=green]"//fmstr)

        ! Map
        call terminal_color(INT8(X'FFCCCCCC'))
        do xi=1,map_w
            do yi=1,map_h
                call terminal_put(xi, yi, ICHAR(game_map(xi, yi)))
            end do
        end do
        call terminal_color(INT8(X'FFFFFFFF'))

        ! Enemies
        do i=1,SIZE(e_list)
            call e_list(i)%con%draw
        end do

        ! Player
        call draw_player

        ! box around map
        call draw_box(0,0,72,25)

        call terminal_refresh()

        !UPDATE!

        ! Regen sword
        if (p%sword .lt. 5) then
            p%sword_timer = p%sword_timer - 1
            if (p%sword_timer .eq. 0) then
                p%sword = p%sword + 1
                p%sword_timer = 3
            end if
        end if

        user_in = terminal_read()
        ! Input
        if (user_in .eq. TK_ESCAPE) then
            quit = .true.
        else if (user_in .eq. TK_RIGHT .and. check_coll(p%x + 1, p%y)) then
            p%x = p%x + 1
            p%dirx = 1
            p%diry = 0
        else if (user_in .eq. TK_LEFT .and. check_coll(p%x - 1, p%y)) then
            p%x = p%x - 1
            p%dirx = -1
            p%diry = 0
        else if (user_in .eq. TK_UP .and. check_coll(p%x, p%y - 1)) then
            p%y = p%y - 1
            p%dirx = 0
            p%diry = -1
        else if (user_in .eq. TK_DOWN .and. check_coll(p%x, p%y + 1)) then
            p%y = p%y + 1
            p%dirx = 0
            p%diry = 1
        else if (user_in .eq. TK_SPACE .and. p%sword .gt. 0) then
            p%sword = p%sword - 1
            p%sword_timer = 3
            do i=1,SIZE(e_list)
                if (e_list(i)%con%x .eq. (p%x + p%dirx) .and. e_list(i)%con%y .eq. (p%y + p%diry)) then
                    e_list(i)%con%hp = e_list(i)%con%hp - 1
                end if
            end do
        end if
        user_in = 0

        ! Update enemies
        do i=1,SIZE(e_list)
            call e_list(i)%con%update
        end do

        ! gen new map if on stairs
        if (game_map(p%x, p%y) .eq. '>') then
            level = level + 1
            deallocate(game_map)
            allocate(game_map(70,24))
            map_w = SIZE(game_map, 1)
            map_h = SIZE(game_map, 2)
            call generate_map(game_map, p%x, p%y)
        end if
    end do

    ! Exit !
    deallocate(game_map)
    call terminal_close()

contains
    subroutine draw_box(xp,yp,w,h)
        implicit none
        integer, value :: xp, yp, w, h
        integer :: x, y
        do x=xp,(xp + w - 1)
            call terminal_put(x, yp, INT(X'2501'))
            call terminal_put(x, yp + h - 1, INT(X'2501'))
        end do
        do y=yp,(yp + h - 1)
            call terminal_put(xp, y, INT(X'2503'))
            call terminal_put(xp + w - 1, y, INT(X'2503'))
        end do
        call terminal_put(xp, yp, INT(X'250F'))
        call terminal_put(xp + w - 1, yp, INT(X'2513'))
        call terminal_put(xp, yp + h - 1, INT(X'2517'))
        call terminal_put(xp + w - 1, yp + h - 1, INT(X'251B'))
    end subroutine

    subroutine draw_player()
        ! Draw Player
        call terminal_put(p%x, p%y, ICHAR('@'))
        if (p%dirx .ne. 0) then
            !call terminal_put(p%x + p%dirx, p%y, ICHAR('|'))
            call terminal_put(p%x, p%y + p%dirx, ICHAR('/'))
        else if (p%diry .ne. 0) then
            !call terminal_put(p%x, p%y + p%diry, ICHAR('-'))
            call terminal_put(p%x - p%diry, p%y, ICHAR('\'))
        end if

    end subroutine
end program rogue_like
