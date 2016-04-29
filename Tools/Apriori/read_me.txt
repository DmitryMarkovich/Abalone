Running Apriori on Mac
Hello everyone,

I've had some trouble running the Apriori-algorithm on my Mac so here's what I did to fix it.

The apriori file located in Tools/Apriori/ is not working on some of the later versions of OSX. So, what you need to do is to copy a file called aprioriMAC from the MATLAB toolbox (i.e. 02450Toolbox_Matlab/Tools/Apriori/aprioriMAC) to your the aforementioned folder and rename it to apriori (replacing a file in the process).
(If you are already running Matlab you just need to rename it).

In some cases (MAC and Linux) you might also need to allow the system to execute the file. This is done by changing the files permissions, which is most easily done in a terminal.

Open a terminal and enter:

cd $PATH_TO_THE_TOOLBOX$/02450Toolbox_$YOUR_LANGUAGE$/Tools/Apriori
chmod +x apriori
(NB! Stuff inside $$ in the above you need to change to what is appropriate for you)
 
You might need to run the last command using sudo
 
Cheers,
Søren