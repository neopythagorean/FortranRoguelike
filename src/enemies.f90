module enemies
    use bearlibterminal
    use map_mod
    use mplayer
    implicit none
    type :: Enemy
        integer :: x, y
        integer :: damage, hp = 1
        integer(8) :: color
        character :: char
    contains
        procedure :: update => update_enemy
        procedure :: draw => draw_enemy
    end type Enemy

    type, extends(Enemy) :: Ambler
    contains
        procedure :: update => update_ambler
    end type Ambler

    type, extends(Enemy) :: Zagger
        integer :: xdir, ydir
    contains
        procedure :: update => update_zagger
    end type Zagger

    type, extends(Enemy) :: Gorgnork
    contains
        procedure :: update => update_gorgnork
    end type Gorgnork

    type EnemyHolder
        class(Enemy), allocatable :: con
    end type EnemyHolder

    type(EnemyHolder), dimension(:), allocatable :: e_list

contains

    subroutine update_enemy(this)
        implicit none
        class(Enemy), intent(inout) :: this
    end subroutine

    subroutine draw_enemy(this)
        implicit none
        class(Enemy), intent(inout) :: this
        call terminal_color(this%color)
        call terminal_put(this%x, this%y, ICHAR(this%char))
        call terminal_color(INT8(X'FFFFFFFF'))
    end subroutine

    subroutine update_ambler(this)
        implicit none
        class(Ambler), intent(inout) :: this
        integer :: dir

        if (this%hp .le. 0) then
            this%color = X'FFFF0000'
            return
        end if

        if (this%x .eq. p%x .and. this%y .eq. p%y) then
            p%hp = p%hp - 1
        end if

        if (RAND() < .1) then
            return
        end if

        if (RAND() < .5) then
            dir = MERGE(-1, 1, RAND() < .5)
            if (check_coll(this%x + dir, this%y)) then
                this%x = this%x + dir
            end if
        else
            dir = MERGE(-1, 1, RAND() < .5)
            if (check_coll(this%x, this%y  + dir)) then
                this%y = this%y + dir
            end if
        end if

    end subroutine

    subroutine update_gorgnork(this)
        implicit none
        class(Gorgnork), intent(inout) :: this
        integer :: dir, diry

        if (this%hp .le. 0) then
            this%color = X'FFFF0000'
            return
        end if

        if (this%x .eq. p%x .and. this%y .eq. p%y) then
            p%hp = p%hp - 1
        end if

        if (ABS(p%x - this%x) .lt. 5 .and. ABS(p%y - this%y) .lt. 5) then
            dir = MERGE(-1, 1, p%x - this%x > 0)
            diry = MERGE(-1, 1, p%y - this%y > 0)
            if (check_coll(this%x + dir, this%y)) then
                this%x = this%x + dir
            else if (check_coll(this%x, this%y + diry)) then
                this%y = this%y + diry
            end if
        else if (RAND() < .5) then
            dir = MERGE(-1, 1, RAND() < .5)
            if (check_coll(this%x + dir, this%y)) then
                this%x = this%x + dir
            end if
        else
            dir = MERGE(-1, 1, RAND() < .5)
            if (check_coll(this%x, this%y  + dir)) then
                this%y = this%y + dir
            end if
        end if

    end subroutine

    subroutine update_zagger(this)
        implicit none
        class(Zagger), intent(inout) :: this

        if (this%hp .le. 0) then
            this%color = X'FFFF0000'
            return
        end if
        
        if (this%x .eq. p%x .and. this%y .eq. p%y) then
            p%hp = p%hp - 1
        end if

        if (check_coll(this%x + this%xdir, this%y)) then
            this%x = this%x + this%xdir
        else if (check_coll(this%x, this%y  + this%ydir)) then
            this%y = this%y + this%ydir
            this%xdir = -1 * this%xdir
        else if (check_coll(this%x, this%y  + (-1 * this%ydir))) then
            this%ydir = -1 * this%ydir
            this%y = this%y + this%ydir
            this%xdir = -1 * this%xdir
        else
            this%xdir = -1 * this%xdir
        end if
    end subroutine

    type(Ambler) function make_ambler(x, y) result(amb_out)
        implicit none
        integer, value :: x, y
        amb_out%x = x
        amb_out%y = y
        amb_out%char = 'A'
        amb_out%color = X'FFFFFF00'
    end function

    type(Zagger) function make_zagger(x, y) result(zag_out)
        implicit none
        integer, value :: x, y
        zag_out%x = x
        zag_out%y = y
        zag_out%xdir = MERGE(-1, 1, RAND() < .5)
        zag_out%ydir = MERGE(-1, 1, RAND() < .5)
        zag_out%char = 'Z'
        zag_out%color = X'FF0000FF'
    end function

    type(Gorgnork) function make_grognork(x, y) result(gorg_out)
        implicit none
        integer, value :: x, y
        gorg_out%x = x
        gorg_out%y = y
        gorg_out%char = 'G'
        gorg_out%color = X'FF00AA00'
    end function

    subroutine add_enemy(e_in, pos)
        class(Enemy) :: e_in
        integer, value :: pos
        if (ALLOCATED(e_list(pos)%con)) then
            deallocate(e_list(pos)%con)
        end if
        allocate(e_list(pos)%con, SOURCE = e_in)
    end subroutine

    subroutine destroy_enemies()
        integer :: i, length
        length = SIZE(e_list)
        do i=1,length
            if(ALLOCATED(e_list(i)%con)) then
                deallocate(e_list(i)%con)
            end if
        end do
        deallocate(e_list)
    end subroutine

end module  enemies
