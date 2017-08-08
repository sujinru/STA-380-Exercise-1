# Part A
We know <br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <i>P(RC)+P(TC)=1</i><br/>

and<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>P(Yes)=P(Yes|RC)∗P(RC)+P(Yes|TC)∗P(TC)</i><br/>

Then we have:<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>0.65=0.5∗0.5+P(Yes|TC)∗0.7</i><br/>

Therefore:<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>P(Yes|TC)=71.4%</i><br/>

# PartB
Since <i>P(disease) + P(nodisease)=1 </i><br/>

<i>P(positive)=P(positive|disease)∗P(disease)+P(positive|nodisease)∗P(nodisease)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=0.993∗0.000025+0.0001∗0.999975<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=0.0001248225</i>

According to Bayes Rules<br/>
<i>
P(disease|positive)=P(disease,positive)/P(positive)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=P(positive|disease)∗P(disease)/P(positive)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=0.993∗0.0000250.0001248225<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=0.1988824</i>

Given a positive test result, there is a 19.89% chance that they have the disease.

Since P(disease|positive) is low, a universal test for the disease may cause unnecessary concerns.
