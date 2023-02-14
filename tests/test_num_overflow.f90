      program main

C  This is the FITSIO cookbook program that contains an annotated listing of
C  various computer programs that read and write files in FITS format
C  using the FITSIO subroutine interface.  These examples are
C  working programs which users may adapt and modify for their own
C  purposes.  This Cookbook serves as a companion to the FITSIO User's
C  Guide that provides more complete documentation on all the
C  available FITSIO subroutines.

C  Call each subroutine in turn:

         call readheader
         call readimage
         print *
         print *,"All the fitsio cookbook routines ran successfully."

      end

C *************************************************************************
      subroutine readheader

C  Print out all the header keywords in all extensions of a FITS file

         integer status,unit,readwrite,blocksize,nkeys,nspace,hdutype,i,j
         character filename*80,record*80

C  The STATUS parameter must always be initialized.
         status=0

C  Get an unused Logical Unit Number to use to open the FITS file.
         call ftgiou(unit,status)

C     name of FITS file
         filename='ATESTFILEZ.FITS'

C     open the FITS file, with read-only access.  The returned BLOCKSIZE
C     parameter is obsolete and should be ignored.
         readwrite=0
         call ftopen(unit,filename,readwrite,blocksize,status)

         j = 0
  100    continue
         j = j + 1

         print *,'Header listing for HDU', j

C  The FTGHSP subroutine returns the number of existing keywords in the
C  current header data unit (CHDU), not counting the required END keyword,
         call ftghsp(unit,nkeys,nspace,status)

C  Read each 80-character keyword record, and print it out.
         do i = 1, nkeys
            call ftgrec(unit,i,record,status)
            print *,record
         end do

C  Print out an END record, and a blank line to mark the end of the header.
         if (status .eq. 0)then
            print *,'END'
            print *,' '
         end if

C  Try moving to the next extension in the FITS file, if it exists.
C  The FTMRHD subroutine attempts to move to the next HDU, as specified by
C  the second parameter.   This subroutine moves by a relative number of
C  HDUs from the current HDU.  The related FTMAHD routine may be used to
C  move to an absolute HDU number in the FITS file.  If the end-of-file is
C  encountered when trying to move to the specified extension, then a
C  status = 107 is returned.
         call ftmrhd(unit,1,hdutype,status)

         if (status .eq. 0)then
C         success, so jump back and print out keywords in this extension
            go to 100

         else if (status .eq. 107)then
C         hit end of file, so quit
            status=0
         end if

C  The FITS file must always be closed before exiting the program.
C  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
         call ftclos(unit, status)
         call ftfiou(unit, status)

C  Check for any error, and if so print out error messages.
C  The PRINTERROR subroutine is listed near the end of this file.
         if (status .gt. 0)call printerror(status)
      end
C *************************************************************************
      subroutine readimage

C  Read a FITS image and determine the minimum and maximum pixel value.
C  Rather than reading the entire image in
C  at once (which could require a very large array), the image is read
C  in pieces, 100 pixels at a time.

         integer status,unit,readwrite,blocksize,naxes(2),nfound
         integer group,firstpix,nbuffer,npixels,i
         real datamin,datamax,nullval,buffer(100)
         logical anynull
         character filename*80

C  The STATUS parameter must always be initialized.
         status=0

C  Get an unused Logical Unit Number to use to open the FITS file.
         call ftgiou(unit,status)

C  Open the FITS file known to contain values of type REAL(8) outside of range of REAL(4)
         filename='ATESTFILEZ.FITS'
         readwrite=0
         call ftopen(unit,filename,readwrite,blocksize,status)

C  Determine the size of the image.
         call ftgknj(unit,'NAXIS',1,2,naxes,nfound,status)

C  Check that it found both NAXIS1 and NAXIS2 keywords.
         if (nfound .ne. 2)then
            print *,'READIMAGE failed to read the NAXISn keywords.'
            return
         end if

C  Initialize variables
         npixels=naxes(1)*naxes(2)
         group=1
         firstpix=1
         nullval=-999
         datamin=1.0E30
         datamax=-1.0E30

         do while (npixels .gt. 0)
C         read up to 100 pixels at a time
            nbuffer=min(100,npixels)

            call ftgpve(unit,group,firstpix,nbuffer,nullval,
     &              buffer,anynull,status)

C         find the min and max values
            do i=1,nbuffer
               datamin=min(datamin,buffer(i))
               datamax=max(datamax,buffer(i))
            end do

C         increment pointers and loop back to read the next group of pixels
            npixels=npixels-nbuffer
            firstpix=firstpix+nbuffer
         end do

         print *
         print *,'Min and max image pixels = ',datamin,datamax

C  The FITS file must always be closed before exiting the program.
C  Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
         call ftclos(unit, status)
         call ftfiou(unit, status)

C  Check for any error, and if so print out error messages.
C  The PRINTERROR subroutine is listed near the end of this file.
         if (status .gt. 0)call printerror(status)
      end

C *************************************************************************
      subroutine printerror(status)

C  This subroutine prints out the descriptive text corresponding to the
C  error status value and prints out the contents of the internal
C  error message stack generated by FITSIO whenever an error occurs.

         integer status
         character errtext*30,errmessage*80

C  Check if status is OK (no error); if so, simply return
         if (status .le. 0)return

C  The FTGERR subroutine returns a descriptive 30-character text string that
C  corresponds to the integer error status number.  A complete list of all
C  the error numbers can be found in the back of the FITSIO User's Guide.
         call ftgerr(status,errtext)
         print *,'FITSIO Error Status =',status,': ',errtext

C  FITSIO usually generates an internal stack of error messages whenever
C  an error occurs.  These messages provide much more information on the
C  cause of the problem than can be provided by the single integer error
C  status value.  The FTGMSG subroutine retrieves the oldest message from
C  the stack and shifts any remaining messages on the stack down one
C  position.  FTGMSG is called repeatedly until a blank message is
C  returned, which indicates that the stack is empty.  Each error message
C  may be up to 80 characters in length.  Another subroutine, called
C  FTCMSG, is available to simply clear the whole error message stack in
C  cases where one is not interested in the contents.
         call ftgmsg(errmessage)
         do while (errmessage .ne. ' ')
            print *,errmessage
            call ftgmsg(errmessage)
         end do
      end
C *************************************************************************
      subroutine deletefile(filename,status)

C  A simple little routine to delete a FITS file

         integer status,unit,blocksize
         character*(*) filename

C  Simply return if status is greater than zero
         if (status .gt. 0)return

C  Get an unused Logical Unit Number to use to open the FITS file
         call ftgiou(unit,status)

C  Try to open the file, to see if it exists
         call ftopen(unit,filename,1,blocksize,status)

         if (status .eq. 0)then
C         file was opened;  so now delete it
            call ftdelt(unit,status)
         else if (status .eq. 103)then
C         file doesn't exist, so just reset status to zero and clear errors
            status=0
            call ftcmsg
         else
C         there was some other error opening the file; delete the file anyway
            status=0
            call ftcmsg
            call ftdelt(unit,status)
         end if

C  Free the unit number for later reuse
         call ftfiou(unit, status)
      end
