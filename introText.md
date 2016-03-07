This application gives a user-friendly interface for the estimation of flood frequency curves when combining historical data with systematic records from peak river flow. 
Information on the systematic peak flow value is given in the left panel below: if the systematic data come from one of the [NRFA](http://nrfa.ceh.ac.uk/peak-flow-data) Peak River Flow stations it can be requested by inserting the gauging station number, alternatively it can be given in the second field as comma separated values. Make sure you delete the station number if the peak flow values are inserted manually. 
The default distribution for peak flow data in the UK is the GLO distribution - currently the methods used in this application have only been developed for the GEV and the GLO distribution.
If there are reasons to think that the data under study come from a GEV distribution - change the selected distribution.
To combine some historical data with the systematic data some information is needed: the historical peak flow values, the length of the time spanned by the historical information (h) and the threshold above which historical data have been recorded (perception threshold X0). 
If the information available is that the historical flows were above the perception threshold and the actual flow values are unknown - indicate that in the options. 
Once you have included all the information available to you (which might be just the systematic data) - push the submit button: results will appear and be updated in the tabs below.


Please note: it might happen that the numerical optimisation underlying the estimation procedure would fail. When this happens some cryptic error message will appear in the panels rather than the expected output. Try changing some of the underlying data and information and see if convergence can be achieved, unluckily this is a difficult step to automate. 
