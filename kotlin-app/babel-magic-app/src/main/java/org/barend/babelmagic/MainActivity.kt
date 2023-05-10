package org.barend.babelmagic

import android.content.Context
import android.content.Intent
import android.graphics.Bitmap
import android.graphics.Color
import android.media.MediaPlayer
import android.net.Uri
import android.os.*
import android.text.InputFilter
import android.text.method.ScrollingMovementMethod
import android.util.Log
import android.view.View
import android.widget.ProgressBar
import android.widget.TextView
import androidx.appcompat.app.AppCompatActivity
import androidx.core.view.isVisible
import org.barend.babelmagic.databinding.ActivityMainBinding
import java.io.File
import java.lang.Integer.min
import java.util.*

data class MessageContent(var detection : LanguageDetection = LanguageDetection(),
                          var native : String = "",
                          var translationVias : String = "",
                          var message : String = "",
                          var error : String = "")
{
    fun clear()
    {
        detection = LanguageDetection()
        native = ""
        translationVias = ""
        message = ""
        error = ""
    }

    override fun toString() : String
    {
        var result = ""
        if (detection.languageCode.isNotEmpty())
        {
            result += "${detection.languageCode} (${detection.score}) -> ${detection.getVia()}"
        }
        if (native.isNotEmpty())
        {
            result += "\nNative: ${native}"
        }
        if (message.isNotEmpty())
        {
            result += "\n${message}"
        }
        if (error.isNotEmpty())
        {
            result += "\n${error}"
        }
        result += "\n${translationVias}"
        return result
    }
}

data class ResultState(var results : MutableList<TranslateResult> = mutableListOf<TranslateResult>(),
                       var lastBareInputText : String = "",
                       var lastScores : Array<Double> = arrayOf()) {}

class MainActivity : AppCompatActivity()
{
    private lateinit var binding : ActivityMainBinding

    // The native, or default, language. This language is used to also show a translation
    // in the language the user knows well.
    private val nativeLanguageCode = Locale.getDefault().language.lowercase()

    private var messageContent = MessageContent()
    private var theState = ResultState()

    private var azureServiceInfo = AzureServiceInfo()

    private var mediaPlayer : MediaPlayer? = null
    private var cacheSpeechFileCounter = 0

    private fun setText(view : TextView, text : String)
    {
        view.text = text
        view.movementMethod = ScrollingMovementMethod()
    }

    private fun setText(view : TextView, text : CharSequence)
    {
        view.text = text
        view.movementMethod = ScrollingMovementMethod()
    }

    private fun reportError(msg : String, inCorrection : Boolean = true)
    {
        Log.e("ERROR", msg)
        messageContent.error = msg
        val colored = createColoredText(this@MainActivity, msg, Color.RED)
        if (inCorrection)
        {
            setText(binding.debugTextView, messageContent.toString())
            setText(binding.correctionTextView, colored)
        }
        else
        {
            // For errors in images, they should not overwrite.
            setText(binding.debugTextView, colored)
        }
    }

    private fun updateMediaButtons()
    {
        binding.mediaPlayButton.isVisible = mediaPlayer != null && !mediaPlayer!!.isPlaying
        binding.mediaPauseButton.isVisible = mediaPlayer != null && mediaPlayer!!.isPlaying
    }

    private fun releaseMediaPlayer()
    {
        if (mediaPlayer != null)
        {
            mediaPlayer!!.release()
            mediaPlayer = null
        }
        updateMediaButtons()
    }

    private fun clearState()
    {
        releaseMediaPlayer()
        messageContent.clear()
        theState = ResultState()
        setText(binding.debugTextView, messageContent.toString())
        setText(binding.correctionTextView, "")
        binding.editTextTextMultiLine.setText("")
        binding.linkTextView.setText("")
    }

    private fun createCallInfo(functionKey : String) : ServiceCallInfo
    {
        val api = resources.getString(R.string.azure_service_api_url)
        val key = resources.getString(R.string.azure_service_api_key)
        val textKey = resources.getString(R.string.azure_service_text_key)
        val ok = api.isNotEmpty() && key.isNotEmpty() && textKey.isNotEmpty()
        return ServiceCallInfo(api, functionKey, key, textKey, enabled = ok)
    }

    private fun initializeUser()
    {
        val fkKey = resources.getString(R.string.azure_service_fk_key)
        val info = createCallInfo(resources.getString(R.string.azure_service_initialize_function_key))

        var userid = ""
        val sharedPref = this@MainActivity.getPreferences(Context.MODE_PRIVATE)
        if (sharedPref != null)
        {
            userid = sharedPref.getString(getString(R.string.userid), "") ?: ""
        }

        Initialize(info, userid, fkKey,
        {
            azureServiceInfo = it
            binding.linkTextView.setText("${azureServiceInfo.userId} ${azureServiceInfo.tsDiff}")

            if (sharedPref != null)
            {
                with (sharedPref.edit()) {
                    putString(getString(R.string.userid), azureServiceInfo.userId)
                    apply()
                }
            }
        },
        {
            reportError(it)
        })
    }

    private fun callTranslateVia(detected : String, via : String, onFinish : (code : String, langCode : String, corr : String, quiteRight : Boolean) -> Unit)
    {
        val info = createCallInfo(azureServiceInfo.fkTv)

        if (! info.enabled)
        {
            reportError("No API url or key available for ${ACTION_TRANSLATE_VIA}")
            onFinish("no api", "", "", false)
        } else
        {
            val s = binding.editTextTextMultiLine.text.toString()

            val par = TranslateViaParameters(language = detected,
                viaLanguage = via,
                nativeLanguage = nativeLanguageCode,
                text = s)
            verifyTextWithTranslateVia(info, par,
                { translateResult, native ->
//                    myLog("VIA","${par} -> ${translateResult}")
                    theState.results.add(translateResult)
                    if (native.isNotEmpty())
                    {
                        messageContent.native = native
                    }
                    val scored = scoreResults(s, theState.results)
                    showResults(s, scored)
                    theState.lastScores = resultsAsScoreArray(scored)
                    onFinish(messageContent.message, detected, stringifyCorrections(scored), isQuiteRight(scored))
                },
                { reportError(it) })
        }
    }

    private fun startTranslateVia(detection : LanguageDetection, index : Int, onFinish : (code : String, langCode : String, c : String) -> Unit)
    {
        if (index == 0)
        {
            theState.results = mutableListOf()
        }
        if (index >= detection.via.size)
        {
            return
        }
        callTranslateVia(detection.languageCode, detection.via[index],
            { cd, lc, corr, quiteRight ->
                if (!quiteRight && index + 1 < detection.via.size)
                {
                    // Using: a Kotlin one shot timer
                    // Schedule next language after a second.
                    // This takes care of quick reponse, and does not block UI
                    // or report it as "busy" with the option to wait.
                    // (but this was caused by permutations/word order)
                    Handler(Looper.getMainLooper()).postDelayed({
                        startTranslateVia(detection, index + 1, onFinish)
                    }, 100)
                }
                else
                {
                    onFinish(cd, lc, corr)
                }
            })
    }

    private fun stopTimer(timer : Timer)
    {
        binding.progressBar.visibility = View.INVISIBLE
        timer.cancel()
    }

    // It creates a timer which updates the progress bar (should be between 0..100)
    // Calling function should cancel the returned timer after the job is done,
    // and set the progress bar to invisible again
    private fun createTimer(pb : ProgressBar, title : String, period : Long) : Timer
    {
        pb.progress = 0
        pb.visibility = View.VISIBLE
        val timer = Timer()
        val tt : TimerTask = object : TimerTask()
        {
            override fun run()
            {
                pb.progress = min(pb.progress + 1, pb.max)
                myLog("TIMER", "progress for ${title}: ${pb.progress}")
                if (pb.progress == pb.max)
                {
                    stopTimer(timer)
                }
            }
        }
        timer.schedule(tt, period, period)
        return timer
    }

    private fun replay()
    {
        val pathName = "${cacheDir}/speech${cacheSpeechFileCounter}.mp3"
        startMediaPlayer(pathName)
    }

    private fun saveAndPlay(ba : ByteArray)
    {
        cacheSpeechFileCounter++
        val pathName = "${cacheDir}/speech${cacheSpeechFileCounter}.mp3"
        myLog(TAG_SPEECH, "Save speech to ${pathName}")
        val file = File(pathName)
        file.writeBytes(ba)
        startMediaPlayer(pathName)
    }

    private fun startSpeech(langCode : String, text : String)
    {
        if (text.isEmpty())
        {
            return
        }

        releaseMediaPlayer()

        val info = createCallInfo(azureServiceInfo.fkTts)

        if (info.enabled)
        {
            getTextToSpeech(info, langCode, text,
                {
                    saveAndPlay(it)
                },
                {
                    reportError((it))
                })
        }
    }

    private fun startMediaPlayer(pathName : String)
    {
        // Release it again, because all is asynchronous. It might be that a previous
        // speech was still on the way. Without releasing, there might be two sounds then.
        releaseMediaPlayer()
        mediaPlayer = MediaPlayer()
        try {
            myLog(TAG_SPEECH, "Play ${pathName}")
            mediaPlayer!!.setDataSource(pathName)
            mediaPlayer!!.isLooping = true
            mediaPlayer!!.prepare()
            mediaPlayer!!.start()
        } catch (e: Exception) {
            e.printStackTrace()
        }
        updateMediaButtons()
    }


    private fun verifyText(detection : LanguageDetection, timer : Timer)
    {
        // Lambda to cancel the timer
        val finish = { resultString : String, langCode : String, corrections : String ->
            println("TIMER Finish with ${resultString}")
            stopTimer(timer)

            startSpeech(langCode, corrections)
        }

        messageContent.clear()
        messageContent.detection = detection
        messageContent.native = detection.native

        if (detection.score >= 0.65)
        {
            // This will be the input language code for DeepL/Azure Service calling DeepL or Translate.
            // For now, we assume these codes always correspond.
            if (detection.supported)
            {
                setText(binding.debugTextView, messageContent.toString())
                startTranslateVia(detection, 0, finish)
            }
            else
            {
                reportError("Language not supported by this app")
                finish("Unsupported", "", "")
            }
        }
        else
        {
            reportError("Language not detected")
            finish("Not detected", "", "")
        }
    }

    private fun detectAndVerifyText(s : String)
    {
        val info = createCallInfo(azureServiceInfo.fkDl)

        val timer = createTimer(binding.progressBar, "DetectAndVerify", 1000)

        if (info.enabled)
        {
            azureDetectLanguage(info, nativeLanguageCode, s,
                { verifyText(it, timer) },
                {
                    timer.cancel()
                    reportError(it)
                }
            )
        }
    }

    // Get the image via the Azure service, which calls unsplash
    private fun getImage()
    {
        val info = createCallInfo(azureServiceInfo.fkRi)
        myLog(TAG_AZURE, info.toString())

        val timer = createTimer(binding.progressBar, "unsplash", 1000)

        val assignImage = { bitmap : Bitmap? ->
            stopTimer(timer)
            binding.imageView.setImageBitmap(bitmap)
            if (bitmap == null)
            {
                binding.linkTextView.text = ""
            }
        }

        val subject = binding.editTextSubject.text.toString()
        if (info.enabled)
        {
            getRandomImage(info, RandomImageParameters(subject, "landscape"),
                {
                    binding.linkTextView.text = it.link
                    retrieveImage(it, assignImage)
                },
                {
                    assignImage(null)
                    reportError(it, false)
                })
        }
    }

    private fun showResults(inputText : String, scored : List<TranslateResult>, source : String = "")
    {
        setText(binding.correctionTextView, colorResults(this@MainActivity, scored))

        messageContent.message = when
        {
            scored.isEmpty() -> "No response"
            bareString(inputText) == bareString(scored[0].backText) -> "All right"
            isQuiteRight(scored) -> "Quite right"
            else -> "Alternatives or corrections"
        } + source
        messageContent.translationVias = scored.toString()

        setText(binding.debugTextView, messageContent.toString())
    }

    private fun handleVerify(inputText : String)
    {
        val bareInputText = bareString(inputText)

        val minLength = if (bareInputText.isNotEmpty() && isCJK(Character.codePointAt(bareInputText, 0))) 3 else INPUT_TEXT_LENGTH_MIN
        if (bareInputText.length < minLength)
        {
            messageContent.clear()
            reportError("Input text is too short (${bareInputText.length}).")
            return
        }
        else if (theState.lastBareInputText.isNotEmpty())
        {
            // If there is not much difference with previous input,
            // then there is no reason to send another request to the service.
            val sim = getStringSimilarityScore(bareInputText, theState.lastBareInputText)
            if (sim > 0.50)
            {
                val log_tag = "BARESIM"

                myLog(log_tag, "The text is maybe the same, ${sim}")
                val scored = scoreResults(inputText, theState.results)
                val improved = isImproved(scored, theState.lastScores)
                val newScores = resultsAsScoreArray(scored)
                myLog(log_tag, "The text is improved? ${improved} : ${scoreString(theState.lastScores)} -> ${scoreString(newScores)}")
                if (improved && sim < 1.0 && scored.isNotEmpty())
                {
                    myLog(log_tag, "Aligned inp ${scored[0].alignedInput}")
                    myLog(log_tag, "Aligned res ${scored[0].alignedResponse}")
                }
                if (improved)
                {
                    showResults(inputText, scored, " [reuse]")
                    theState.lastScores = newScores
                    theState.lastBareInputText = bareInputText
                    replay()
                    return
                }
            }
        }

        releaseMediaPlayer()
        theState.lastScores = arrayOf()
        theState.lastBareInputText = bareInputText
        detectAndVerifyText(inputText)
    }


    override fun onCreate(savedInstanceState : Bundle?)
    {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)


        binding.getImageButton.setOnClickListener {
            clearState()
            getImage()
        }

        binding.buttonLangTool.setOnClickListener {
            handleVerify(binding.editTextTextMultiLine.text.toString())
       }

        binding.mediaPlayButton.setOnClickListener {
            if (mediaPlayer != null && !mediaPlayer!!.isPlaying)
            {
               mediaPlayer!!.start()
            }
            updateMediaButtons()
        }

        binding.mediaPauseButton.setOnClickListener {
            if (mediaPlayer != null && mediaPlayer!!.isPlaying)
            {
                mediaPlayer!!.pause()
            }
            updateMediaButtons()
        }

        binding.linkTextView.setOnClickListener {
            val url = binding.linkTextView.text.toString()
            if (url.isNotEmpty())
            {
                try { startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(url))) } catch(e : Exception) {}
            }
        }

        initializeUser()

        binding.editTextTextMultiLine.setFilters(arrayOf(InputFilter.LengthFilter(INPUT_TEXT_LENGTH_MAX)))
        binding.editTextSubject.setFilters(arrayOf(InputFilter.LengthFilter(SUBJECT_LENGTH_MAX)))
        clearState()
        clearCache(cacheDir)
    }

    private fun isCJK(codepoint : Int) : Boolean
    {
        return Character.UnicodeScript.of(codepoint) == Character.UnicodeScript.HAN
    }

}
