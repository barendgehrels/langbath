package org.barend.babelmagic

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Handler
import android.os.Looper
import android.util.Log
import java.io.FileNotFoundException
import java.util.*
import java.util.concurrent.Executors
import javax.net.ssl.HttpsURLConnection

private const val TAG = "RETRIEVE_IMAGE"

// This function (assuming not being called from the main thread) retrieves an image
// from a specified URL and calls the lambda in the main UI thread (to assign it)
private fun doRetrieveImage(imageURL : String, processResult : (bm : Bitmap?) -> Unit)
{
    try
    {
        val url = java.net.URL(imageURL)
        val connection : HttpsURLConnection = url.openConnection() as HttpsURLConnection
        connection.requestMethod = "GET"
        val inputStream = connection.inputStream
        val bitmap = BitmapFactory.decodeStream(inputStream)
        inputStream.close()

        Handler(Looper.getMainLooper()).post {
            processResult(bitmap)
            if (bitmap != null)
            {
                Log.i(TAG, "Image retrieved: ${bitmap.width} x ${bitmap.height}")
            } else
            {
                Log.e(TAG, "Not found: ${imageURL}")
            }
        }
    } catch (e : FileNotFoundException)
    {
        Log.e(TAG, "Not found: ${e.message}")
    } catch (e : Exception)
    {
        Log.e(TAG, "Exception: ${e.message}")
    }
}

// Retrieve an image from a specified URL.
// This function (can be called from UI thread).
// Retrieval is done asynchronously in another thread. On reception of the image,
// calls the lambda.
fun retrieveImage(url : String, processResult : (bm : Bitmap?) -> Unit)
{
    Log.i(TAG, "Retrieve image from url: ${url}")
    if (url.isEmpty())
    {
        processResult(null)
        return;
    }
    val executor = Executors.newSingleThreadExecutor()
    executor.execute {
        doRetrieveImage(url, processResult)
    }
}

fun retrieveImage(info : ImageInfo, processResult : (bm : Bitmap?) -> Unit)
{
    // Ask for the regular resolution image
    retrieveImage(info.url, processResult)

    // Already display the thumbnail for cases where getting the image takes long time
    // (sometimes, somehow getting the image takes nearly a minute - while other traffic is fast...)
    if (info.thumbnail.isNotEmpty())
    {
        try
        {
            val decoder = Base64.getDecoder()
            val bytes = decoder.decode(info.thumbnail)
            val bitmap = BitmapFactory.decodeByteArray(bytes, 0, bytes.size)
            processResult(bitmap)
        } catch (e : Exception)
        {
            Log.e(TAG, "Exception: ${e.message}")
        }
    }
}
