package org.barend.babelmagic

import android.util.Log
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

// Removes .mp3 files from the specified folder, if any.
fun clearCache(folder : File)
{
    // From the docs: Even though Android sometimes deletes cache files on its own,
    // you shouldn't rely on the system to clean up these files for you.

    val files = mutableListOf<String>()

    folder.walk().forEach {
        val filename = it.absolutePath
        if (filename.endsWith(".mp3"))
        {
            files.add(filename)
        }
    }

    for (file in files)
    {
        Files.deleteIfExists(Paths.get(file))
    }

    Log.i(TAG_SPEECH, "Deleted: ${files.size} mp3 files from the app cache.")
}
