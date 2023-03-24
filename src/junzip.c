// JUnzip library by Joonas Pihlajamaa. See junzip.h for license and details.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "junzip.h"

// Read local ZIP file header. Silent on errors so optimistic reading possible.
int jzReadLocalFileHeaderRaw(JZFile *zip, JZLocalFileHeader *header,
                             char *filename, int len)
{
    if (zip->read(zip, header, sizeof(JZLocalFileHeader)) <
        sizeof(JZLocalFileHeader))
        return Z_ERRNO;

    if (header->signature != 0x04034B50)
        return Z_ERRNO;

    if (len)
    { // read filename
        if (header->fileNameLength >= len)
            return Z_ERRNO; // filename cannot fit

        if (zip->read(zip, filename, header->fileNameLength) <
            header->fileNameLength)
            return Z_ERRNO; // read fail

        filename[header->fileNameLength] = '\0'; // NULL terminate
    }
    else
    { // skip filename
        return Z_ERRNO;
    }

    if (header->extraFieldLength)
    {
        char extraField[header->extraFieldLength];

        if (zip->read(zip, extraField, header->extraFieldLength) <
            header->extraFieldLength)
            return Z_ERRNO; // read fail
    }

    // For now, silently ignore bit flags and hope ZLIB can uncompress
    // if(header->generalPurposeBitFlag)
    //     return Z_ERRNO; // Flags not supported

    if (header->compressionMethod == 0 &&
        (header->compressedSize != header->uncompressedSize))
        return Z_ERRNO; // Method is "store" but sizes indicate otherwise, abort

    return Z_OK;
}

int jzReadLocalFileHeader(JZFile *zip, JZFileHeader *header,
                          char *filename, int len)
{
    JZLocalFileHeader localHeader;

    if (jzReadLocalFileHeaderRaw(zip, &localHeader, filename, len) != Z_OK)
        return Z_ERRNO;

    memcpy(header, &localHeader.compressionMethod, sizeof(JZFileHeader));
    header->offset = 0; // not used in local context

    return Z_OK;
}

// Read data from file stream, described by header, to preallocated buffer
int jzReadData(JZFile *zip, JZFileHeader *header, void *buffer)
{
    unsigned char jzBuffer[JZ_BUFFER_SIZE]; // limits maximum zip descriptor size

#ifdef HAVE_ZLIB
    unsigned char *bytes = (unsigned char *)buffer; // cast
    long compressedLeft, uncompressedLeft;
    int ret;
    z_stream strm;
#endif

    if (header->compressionMethod == 0)
    { // Store - just read it
        if (zip->read(zip, buffer, header->uncompressedSize) <
                header->uncompressedSize ||
            zip->error(zip))
            return Z_ERRNO;
#ifdef HAVE_ZLIB
    }
    else if (header->compressionMethod == 8)
    { // Deflate - using zlib
        strm.zalloc = Z_NULL;
        strm.zfree = Z_NULL;
        strm.opaque = Z_NULL;

        strm.avail_in = 0;
        strm.next_in = Z_NULL;

        // Use inflateInit2 with negative window bits to indicate raw data
        if ((ret = inflateInit2(&strm, -MAX_WBITS)) != Z_OK)
            return ret; // Zlib errors are negative

        // Inflate compressed data
        for (compressedLeft = header->compressedSize,
            uncompressedLeft = header->uncompressedSize;
             compressedLeft && uncompressedLeft && ret != Z_STREAM_END;
             compressedLeft -= strm.avail_in)
        {
            // Read next chunk
            strm.avail_in = zip->read(zip, jzBuffer,
                                      (sizeof(jzBuffer) < compressedLeft) ? sizeof(jzBuffer) : compressedLeft);

            if (strm.avail_in == 0 || zip->error(zip))
            {
                inflateEnd(&strm);
                return Z_ERRNO;
            }

            strm.next_in = jzBuffer;
            strm.avail_out = uncompressedLeft;
            strm.next_out = bytes;

            compressedLeft -= strm.avail_in; // inflate will change avail_in

            ret = inflate(&strm, Z_NO_FLUSH);

            if (ret == Z_STREAM_ERROR)
                return ret; // shouldn't happen

            switch (ret)
            {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR; /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(&strm);
                return ret;
            }

            bytes += uncompressedLeft - strm.avail_out; // bytes uncompressed
            uncompressedLeft = strm.avail_out;
        }

        inflateEnd(&strm);
#else
#ifdef HAVE_PUFF
    }
    else if (header->compressionMethod == 8)
    { // Deflate - using puff()
        unsigned long destlen = header->uncompressedSize,
                      sourcelen = header->compressedSize;
        unsigned char *comp = (unsigned char *)malloc(sourcelen);
        if (comp == NULL)
            return Z_ERRNO; // couldn't allocate
        unsigned long read = zip->read(zip, comp, sourcelen);
        if (read != sourcelen)
            return Z_ERRNO; // TODO: more robust read loop
        int ret = puff((unsigned char *)buffer, &destlen, comp, &sourcelen);
        free(comp);
        if (ret)
            return Z_ERRNO; // something went wrong
#endif // HAVE_PUFF
#endif
    }
    else
    {
        return Z_ERRNO;
    }

    return Z_OK;
}

typedef struct
{
    JZFile handle;
    FILE *fp;
} StdioJZFile;

static size_t
stdio_read_file_handle_read(JZFile *file, void *buf, size_t size)
{
    StdioJZFile *handle = (StdioJZFile *)file;
    return fread(buf, 1, size, handle->fp);
}

static size_t
stdio_read_file_handle_tell(JZFile *file)
{
    StdioJZFile *handle = (StdioJZFile *)file;
    return ftell(handle->fp);
}

static int
stdio_read_file_handle_seek(JZFile *file, size_t offset, int whence)
{
    StdioJZFile *handle = (StdioJZFile *)file;
    return fseek(handle->fp, offset, whence);
}

static int
stdio_read_file_handle_error(JZFile *file)
{
    StdioJZFile *handle = (StdioJZFile *)file;
    return ferror(handle->fp);
}

static void
stdio_read_file_handle_close(JZFile *file)
{
    StdioJZFile *handle = (StdioJZFile *)file;
    fclose(handle->fp);
    free(file);
}

JZFile *
jzfile_from_stdio_file(FILE *fp)
{
    StdioJZFile *handle = (StdioJZFile *)malloc(sizeof(StdioJZFile));

    if (handle == NULL)
        return NULL;

    handle->handle.read = stdio_read_file_handle_read;
    handle->handle.tell = stdio_read_file_handle_tell;
    handle->handle.seek = stdio_read_file_handle_seek;
    handle->handle.error = stdio_read_file_handle_error;
    handle->handle.close = stdio_read_file_handle_close;
    handle->fp = fp;

    return &(handle->handle);
}
