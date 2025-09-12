### **Module:** ***Load data***

**BACKGROUND**

MetaInsight allows data in either long format, or wide format. This tab provides instructions which are updated depending on the options selected.

The maximum number of arms for each trial allowed is six and studies cannot contain multiple arms of the same treatment.

Default datasets in each format can be loaded by clicking the *Load example data* button or downloaded by clicking the *Download example data* button.

Any errors in the uploaded data will be displayed and logged in the logger.

Once data is loaded successfully, it will be displayed in the Data table tab.

<p style = "color:red">
Files used before version 5.0 are no longer compatible. To use an older file, use the <i>Data upgrade</i> module, or remove the "StudyID" column and replace the 
numeric treatment IDs in the "T" column(s) with the treatment names.
<p/>

<div class="long_guidance">
<h2><strong>Instructions for uploading long format data</strong></h2>
<div class="continuous_guidance">
<p>The long format data file should contain the following columns:</p>
<ul>
<li>
  <strong>Study</strong>
   contains the name (e.g., author, year) of the study. The study name must be unique for each study.
</li>
</ul>
<ul>
<li>
  <strong>T</strong>
   contains the name of the treatment used in each arm of the study. Treatment names should only contain letters, numbers and underscores.
</li>
</ul>
<ul>
<li>
  <strong>N</strong>
   contains the number of participants in each arm of the study.
</li>
</ul>
<ul>
<li>
  <strong>Mean</strong>
   contains the mean value of the outcome in each arm of the study.
</li>
</ul>
<ul>
<li>
  <strong>SD</strong>
   contains the standard deviation of the outcome in each arm of the study.
</li>
</ul>
</div>
<div class="binary_guidance" style = "display: none;">
<p>The long format data file should contain the following columns:</p>
<ul>
<li>
  <strong>Study</strong>
   contains the name (e.g., author,year) of the study. The study name must be unique for each study.
</li>
</ul>
<ul>
<li>
  <strong>T</strong>
   contains the name of the treatment used in each arm of the study. Treatment names should only contain letters, numbers and underscores.
</li>
</ul>
<ul>
<li>
  <strong>R</strong>
   contains the number of participants with the outcome of interest in each arm of the study.
</li>
</ul>
<ul>
<li>
  <strong>N</strong>
   contains the number of participants in each arm of the study.
</li>
</ul>
<p>N.B. Continuity corrections will need to be applied to cells containing 0 values</p>
</div>
<p>The long format data file may also contain the following columns:</p>
<ul>
<li>
<strong>covar.&lt;COVARIATE_NAME&gt;</strong>
 contains the study-level covariate value, where &lt;COVARIATE_NAME&gt; is replaced by the name of the covariate. This must be identical for each arm of the study. The name of the covariate will be extracted and used in the analysis output.
</li>
</ul>
<ul>
<li>
<strong>rob</strong>
 contains the risk of bias of the study as required by CINeMA (https://cinema.ispm.unibe.ch/). This must be identical for each arm of the study. Low risk = 1, medium = 2, high = 3.
</li>
</ul>
<ul>
<li>
<strong>indirectness</strong>
 contains the indirectness of the study as required by CINeMA (https://cinema.ispm.unibe.ch/). This must be identical for each arm of the study. Low indirectness = 1, medium = 2, high = 3.
</li>
</ul>
<ul>
<li>
<strong>rob.&lt;ROB_NAME_1&gt;, ... , rob.&lt;ROB_NAME_10&gt;</strong>
 contain individual risk of bias components. Up to 10 such columns can be present. Low risk = 1, medium = 2, high = 3.
</li>
</ul>
</div>


<div class="wide_guidance" style = "display: none;">

<h2><strong>Instructions for uploading wide format data</strong></h2>
                            

<p>The wide format data file should contain the following columns:</p>
<p>
  </p><ul>
    <li>
      <strong>Study</strong>
      contains name (e.g., author,year) of the study. The study name must be unique for each study.
    </li>
  </ul>
<p></p>
<p>
  </p><ul>
    <li>
      <strong>T.1, T.2, ..., up to T.6</strong>
      contains name of the treatment given for study arm 1, 2, ..., up to 6, respectively. Treatment names should only contain letters, numbers and underscores.
    </li>
  </ul>
<p></p>
<div class="continuous_guidance" >
  <p>
    </p><ul>
      <li>
        <strong>N.1, N.2, ..., up to N.6</strong>
        contains number of participants in study arm 1, 2, ..., up to 6, respectively
      </li>
    </ul>
  <p></p>
  <p>
    </p><ul>
      <li>
        <strong>Mean.1, Mean.2, ..., up to Mean.6</strong>
        contains the mean value of the outcome in study arm 1, 2, ..., up to 6, respectively
      </li>
    </ul>
  <p></p>
  <p>
    </p><ul>
      <li>
        <strong>SD.1, SD.2, ..., up to SD.6</strong>
        contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively
      </li>
    </ul>
  <p></p>
</div>
<div class="binary_guidance" style = "display: none;">
  <p>
    </p><ul>
      <li>
        <strong>R.1, R.2, ..., up to R.6</strong>
        contains number of participants with the outcome of interest in study arm 1, 2, ..., up to 6, respectively
      </li>
    </ul>
  <p></p>
  <p>
    </p><ul>
      <li>
        <strong>N.1, N.2, ..., up to N.6</strong>
        contains number of participants in study arm 1, 2, ..., up to 6, respectively
      </li>
    </ul>
  <p></p>
</div>
<p>The wide format data file may also contain the following columns:</p>
<ul>
  <li>
    <strong>covar.&lt;COVARIATE_NAME&gt;</strong>
     contains the study-level covariate value, where &lt;COVARIATE_NAME&gt; is replaced by the name of the covariate. The name of the covariate will be extracted and used in the analysis output.
  </li>
</ul>
<ul>
<li>
<strong>rob</strong>
 contains the risk of bias of the study as required by CINeMA (https://cinema.ispm.unibe.ch/). This must be identical for each arm of the study. Low risk = 1, medium = 2, high = 3.
</li>
</ul>
<ul>
<li>
<strong>indirectness</strong>
 contains the indirectness of the study as required by CINeMA (https://cinema.ispm.unibe.ch/). This must be identical for each arm of the study. Low indirectness = 1, medium = 2, high = 3.
</li>
</ul>
<ul>
<li>
<strong>rob.&lt;ROB_NAME_1&gt;, ... , rob.&lt;ROB_NAME_10&gt;</strong>
 contain individual risk of bias components. Up to 10 such columns can be present. Low risk = 1, medium = 2, high = 3.
</li>
</ul>
</div>
</div>

**REFERENCES**
<div class="continuous_guidance" >
  <p>
  This default dataset for continuous outcome data is a reduced version of the data from Brett Doleman, Ole Mathiesen, Alex J Sutton, Nicola J Cooper, Jon N Lund, John P Williams (2023),
  <em>Non-opioid analgesics for the prevention of chronic postsurgical pain: a systematic review and network meta-analysis</em>
  Br J Anaesth 2023 Jun;130(6):719-728. doi: 10.1016/j.bja.2023.02.041. The outcome is pain on a scale of 0 to 10 and the covariate is the mean age of the participants.
  </p>
</div>
<div class="binary_guidance" style = "display: none;">
  <p>
  This default dataset for binary outcome data is from S. Dias, A.J. Sutton, N.J. Welton, and A.E. Ades (2013b),
  <em>Heterogeneity - Subgroups, Meta-Regression, Bias, and Bias-Adjustment</em>
  , Medical Decision Making 33(5):618-640. The outcome is ACR-50, a reduction of at least 50% in the American College of Rhematology score, and the covariate is the mean disease duration of the participants.
  </p>
</div>

