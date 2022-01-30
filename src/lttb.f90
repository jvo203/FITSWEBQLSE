module lttb
    implicit none

    contains

subroutine lttb_downsize(data, threshold, series)
        implicit NONE

        real, intent(in) :: data(:)
        integer, intent(in) :: threshold
        real, intent(out), allocatable :: series(:)

        ! internal variables
        integer i, sampledIndex, a, nextA, dataLength
        real maxAreaPoint, maxArea, area, every        

        dataLength = size(data)

        ! just copy the data 'as-is'
        if (dataLength .le. threshold) then
            print *, 'no downsizing needed, copying time-series'

            allocate(series(dataLength))
            series(:) = data(:)    

            return
        end if

        print *, 'downsizing time-series with Largest-Triangle-Three-Buckets'

        allocate (series(threshold))

        ! always add the first point
        series(1) = data(1)
        sampledIndex = 1
        a = 0

        ! Bucket size. Leave room for start and end data points
        every = real(dataLength - 2)/real(threshold - 2)

        do i = 0, threshold - 2 - 1
            block
                integer avgRangeStart, avgRangeEnd, avgRangeLength
                real avgX, avgY, pointAX, pointAY
                integer rangeOffs, rangeTo

                avgRangeStart = floor((i + 1)*every) + 1
                avgRangeEnd = floor((i + 2)*every) + 1
                avgRangeEnd = min(avgRangeEnd, dataLength)
                avgRangeLength = avgRangeEnd - avgRangeStart

                avgX = 0.0
                avgY = 0.0

                do while (avgRangeStart < avgRangeEnd)
                    avgX = avgX + avgRangeStart
                    avgY = avgY + data(1 + avgRangeStart)
                    avgRangeStart = avgRangeStart + 1
                end do

                avgX = avgX/avgRangeLength
                avgY = avgY/avgRangeLength

                ! Get the range for this bucket
                rangeOffs = floor((i + 0)*every) + 1
                rangeTo = floor((i + 1)*every) + 1

                ! Point a
                pointAX = a
                pointAY = data(1 + a)

                maxArea = -1

                do while (rangeOffs < rangeTo)
                    ! Calculate triangle area over three buckets
                    area = abs((pointAX - avgX)*(data(1 + rangeOffs) - pointAY) -&
                    &(pointAX - rangeOffs)*(avgY - pointAY))*0.5

                    if (area .gt. maxArea) then
                        maxArea = area
                        maxAreaPoint = data(1 + rangeOffs)
                        nextA = rangeOffs ! Next a is this b
                    end if

                    rangeOffs = rangeOffs + 1
                end do

                series(1 + sampledIndex) = maxAreaPoint ! Pick this point from the bucket
                sampledIndex = sampledIndex + 1
                a = nextA ! This a is the next a (chosen b)
            end block
        end do

        ! always add the last element
        series(1 + sampledIndex) = data(dataLength)
        sampledIndex = sampledIndex + 1

    end subroutine lttb_downsize

end module lttb