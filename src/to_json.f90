subroutine to_json(str_val)
        use json_module
        implicit NONE

        CHARACTER(kind=json_CK, len=:), allocatable, intent(out) :: str_val

        type(json_core) :: json
        type(json_value), pointer :: p

        ! initialize the class
        call json%initialize(non_normal_mode=2)

        ! initialize the structure:
        call json%create_object(p, '')

        ! FITS HEADER
        ! call json%add(p, 'HEADER', char(item%hdr))
        call json%add(p, 'HEADER', 'N/A')

        ! misc. values
        call json%add(p, 'width', item%naxes(1))
        call json%add(p, 'height', item%naxes(2))
        call json%add(p, 'depth', item%naxes(3))
        call json%add(p, 'polarisation', item%naxes(4))
        call json%add(p, 'filesize', 0)
        call json%add(p, 'IGNRVAL', item%ignrval)

        call json%add(p, 'CD1_1', item%cd1_1)
        call json%add(p, 'CD1_2', item%cd1_2)
        call json%add(p, 'CD2_1', item%cd2_1)
        call json%add(p, 'CD2_2', item%cd2_2)

        call json%add(p, 'CRVAL1', item%crval1)
        call json%add(p, 'CDELT1', item%cdelt1)
        call json%add(p, 'CRPIX1', item%crpix1)
        call json%add(p, 'CUNIT1', trim(item%cunit1))
        call json%add(p, 'CTYPE1', trim(item%ctype1))

        call json%add(p, 'CRVAL2', item%crval2)
        call json%add(p, 'CDELT2', item%cdelt2)
        call json%add(p, 'CRPIX2', item%crpix2)
        call json%add(p, 'CUNIT2', trim(item%cunit2))
        call json%add(p, 'CTYPE2', trim(item%ctype2))

        call json%add(p, 'CRVAL3', item%crval3)
        call json%add(p, 'CDELT3', item%cdelt3)
        call json%add(p, 'CRPIX3', item%crpix3)
        call json%add(p, 'CUNIT3', trim(item%cunit3))
        call json%add(p, 'CTYPE3', trim(item%ctype3))

        call json%add(p, 'BMAJ', item%bmaj)
        call json%add(p, 'BMIN', item%bmin)
        call json%add(p, 'BPA', item%bpa)

        call json%add(p, 'BUNIT', trim(item%bunit))
        call json%add(p, 'BTYPE', trim(item%btype))
        call json%add(p, 'SPECSYS', trim(item%specsys))

        call json%add(p, 'RESTFRQ', item%restfrq)
        call json%add(p, 'OBSRA', item%obsra)
        call json%add(p, 'OBSDEC', item%obsdec)

        call json%add(p, 'OBJECT', trim(item%object))
        call json%add(p, 'DATEOBS', trim(item%date_obs))
        call json%add(p, 'TIMESYS', trim(item%timesys))
        call json%add(p, 'LINE', trim(item%line))
        call json%add(p, 'FILTER', trim(item%filter))

        call json%add(p, 'mean_spectrum', item%mean_spectrum)
        call json%add(p, 'integrated_spectrum', item%integrated_spectrum)

        ! statistics (image histogram)
        call json%add(p, 'histogram', item%hist)

        ! print out JSON
        ! call json%print(p)

        ! write the file:
        ! call json%print(p, char(item%datasetid)//'.json')

        ! serialize to string prior to further handling
        call json%serialize(p, str_val)

        ! cleanup:
        call json%destroy(p)

        if (json%failed()) stop 1

    end subroutine to_json
