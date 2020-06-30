module mplayer
    implicit none
    type Player
        integer :: x, y, dirx = 1, diry = 0
        integer :: hp = 5
        integer :: sword = 5, sword_timer = 3
        character(len=30) :: name
    end type Player

    type(Player) :: p

end module mplayer
