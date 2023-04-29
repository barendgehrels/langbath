package org.barend.babelmagic

import java.io.*
import java.net.URL
import javax.net.ssl.HttpsURLConnection

// Retrieves a text from a specified URL.
// There are many versions on the web.
// However, this is the only one I found which works correctly for all of:
//  - DeepL
//  - Cyrillic input
//  - LanguageTool
// https://stackoverflow.com/questions/46177133/http-request-in-android-with-kotlin
// The caller should take care of exceptions.
fun retrieveText(url : URL, postData : String) : String
{
    // Create a post connection
    val connection : HttpsURLConnection = url.openConnection() as HttpsURLConnection
    connection.requestMethod = "POST"
    connection.doOutput = true

    // Send the text to post
    val outputStream : OutputStream = connection.outputStream
    val outputWriter = OutputStreamWriter(outputStream)
    outputWriter.write(postData)
    outputWriter.flush()

    // Read into the response using an input stream
    val response = StringBuffer()
    BufferedReader(InputStreamReader(connection.inputStream)).use {
        var inputLine = it.readLine()
        while (inputLine != null)
        {
            response.append(inputLine)
            inputLine = it.readLine()
        }
        it.close()
    }
    connection.disconnect()

    return response.toString()
}
