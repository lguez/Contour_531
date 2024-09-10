module convert_to_irreg_coord_m

  implicit none

contains

  pure function convert_to_irreg_coord(x, nodes, steps)

    real, intent(in):: x(:), nodes(:), steps(:)
    real convert_to_irreg_coord(size(x))

    ! Local:
    integer k(size(x))

    !---------------------------------------------------

    k = floor(x)
    convert_to_irreg_coord = nodes(k) + (x - k) * steps(k)

  end function convert_to_irreg_coord

end module convert_to_irreg_coord_m
