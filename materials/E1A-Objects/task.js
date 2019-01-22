/*
 * Every item in trialArray represents a single trial
 * trialArray properties include any information needed to present the stim and
 * any trial information needed for data analysis
 */


//////////////// Stim & Trial Array Creation //////////////////////////////////


tempSets = [
    [
        ["b", "b", "g"],
        ["hat", "chair", "chair"],
        ["100", "50", "0"]
    ],
    [
        ["b", "b", "g"],
        ["chair", "hat", "hat"],
        ["100", "50", "0"]
    ],
    [
        ["b", "g", "g"],
        ["hat", "hat", "chair"],
        ["100", "50", "0"]
    ],
    [
        ["b", "g", "g"],
        ["chair", "chair", "hat"],
        ["100", "50", "0"]
    ],
    [
        ["b", "b", "g"],
        ["hat", "chair", "chair"],
        ["0", "50", "100"]
    ],
    [
        ["b", "b", "g"],
        ["chair", "hat", "hat"],
        ["0", "50", "100"]
    ],
    [
        ["b", "g", "g"],
        ["hat", "hat", "chair"],
        ["0", "50", "100"]
    ],
    [
        ["b", "g", "g"],
        ["chair", "chair", "hat"],
        ["0", "50", "100"]
    ]

];


tempSets = shuffle(tempSets);
finalSet = tempSets[0];

if (finalSet[0][0] === finalSet[0][1]) {
    countOrder = [
        ["color", finalSet[0][1], finalSet[2][0]],
        ["identity", finalSet[1][1], finalSet[2][2]]
    ];
} else {
    countOrder = [
        ["identity", finalSet[1][1], finalSet[2][0]],
        ["color", finalSet[0][1], finalSet[2][2]]
    ];
}

countOrder = shuffle(countOrder);

imageList = [
    "chairb.jpg", "chairg.jpg", "hatb.jpg", "hatg.jpg",
    "LL.png", "LR.png", "RR.png", "RL.png"
];
/* creates arrays of the congruent and incongruent item combinations */

flankerCon = ["LL", "RR"];
flankerInc = ["LR", "RL"];

conSet = [];
incSet = [];
temp = [];

for (i = 0; i <= 2; i++) {
    for (n = 0; n <= 1; n++) {
        for (m = 0; m <= 2; m++) {
            temp.push(finalSet[m][i]);
        }
        temp.push(flankerCon[n], "con");
        conSet.push(temp);
        temp = [];
    }
}

for (i = 0; i <= 2; i++) {
    for (n = 0; n <= 1; n++) {
        for (m = 0; m <= 2; m++) {
            temp.push(finalSet[m][i]);
        }
        temp.push(flankerInc[n], "inc");
        incSet.push(temp);
        temp = [];
    }
}


/* Creates the trial array
 * The experiment must have two halves, counting a different feature on each half
 * Each block is 12 normal trials, 1 quiz trial // the nblocks = number of blocks per half
 * Each trial is stored in an array with the accompanying properties
 */
trialArray = [];
for (half = 0; half <= 1; half++) {
    for (nblocks = 0; nblocks <= 11; nblocks++) {
        block = [];
        /* create 100% con */
        for (ntrials = 0; ntrials <= 1; ntrials++) {
            for (i = 0; i <= 5; i++) {
                if (conSet[i][2] == "100") {
                    temp = {
                        color: conSet[i][0],
                        identity: conSet[i][1],
                        proportion: conSet[i][2],
                        flanker: conSet[i][3],
                        target: conSet[i][3][1],
                        distractor: conSet[i][3][0],
                        congruency: conSet[i][4],
                        countCat: countOrder[half][0],
                        countFeat: countOrder[half][1],
                        countProp: countOrder[half][2]
                    };
                    if (temp.target == "L") {
                        temp.cResponse = "z";
                    } else {
                        temp.cResponse = "m";
                    }
                    block.push(temp);
                }
            }
        }
        /* end create 100% con */
        /* create 0% con */
        for (ntrials = 0; ntrials <= 1; ntrials++) {
            for (i = 0; i <= 5; i++) {
                if (incSet[i][2] == "0") {
                    temp = {
                        color: incSet[i][0],
                        identity: incSet[i][1],
                        proportion: incSet[i][2],
                        flanker: incSet[i][3],
                        target: incSet[i][3][1],
                        distractor: incSet[i][3][0],
                        congruency: incSet[i][4],
                        countCat: countOrder[half][0],
                        countFeat: countOrder[half][1],
                        countProp: countOrder[half][2]
                    };
                    if (temp.target == "L") {
                        temp.cResponse = "z";
                    } else {
                        temp.cResponse = "m";
                    }
                    block.push(temp);
                }
            }
        }

        /* end create 0% con */

        /* create 50% con */
        for (i = 0; i <= 5; i++) {
            if (incSet[i][2] == "50") {
                temp = {
                    color: incSet[i][0],
                    identity: incSet[i][1],
                    proportion: incSet[i][2],
                    flanker: incSet[i][3],
                    target: incSet[i][3][1],
                    distractor: incSet[i][3][0],
                    congruency: incSet[i][4],
                    countCat: countOrder[half][0],
                    countFeat: countOrder[half][1],
                    countProp: countOrder[half][2]
                };
                if (temp.target == "L") {
                    temp.cResponse = "z";
                } else {
                    temp.cResponse = "m";
                }
                block.push(temp);
            }
        }

        for (i = 0; i <= 5; i++) {
            if (conSet[i][2] == "50") {
                temp = {
                    color: conSet[i][0],
                    identity: conSet[i][1],
                    proportion: conSet[i][2],
                    flanker: conSet[i][3],
                    target: conSet[i][3][1],
                    distractor: conSet[i][3][0],
                    congruency: conSet[i][4],
                    countCat: countOrder[half][0],
                    countFeat: countOrder[half][1],
                    countProp: countOrder[half][2]
                };
                if (temp.target == "L") {
                    temp.cResponse = "z";
                } else {
                    temp.cResponse = "m";
                }
                block.push(temp);
            }
        }
        /* end create 50% con */

        /// adds a quiz trial randomly spliced in somewhere between trial 6 and 12 ///
        quizTrial = [6, 7, 8, 9, 10, 11];
        shuffle(quizTrial);

        quiz = {
            target: "quiz",
        };

        shuffle(block);
        //Adds one quiz trial per 12 trial block
        block.splice(quizTrial[0], 0, quiz);
        trialArray = trialArray.concat(block);
    }
    //Adds one quiz trial at the end of each experiment half
    quiz = {
        target: "quiz",
    };
    trialArray.push(quiz);
}



// Determine the correct response for each of the quiz trials
// loops through and counts the trials that contain the count feature
temp = 0;
for (i = 0; i <= trialArray.length - 1; i++) {
    if (trialArray[i].target === "quiz") {
        trialArray[i].cResponse = temp;
        temp = 0;
    } else {
        if (trialArray[i].countCat === "identity") {
            if (trialArray[i].identity === trialArray[i].countFeat) {
                temp++;
            }
        } else {
            if (trialArray[i].color === trialArray[i].countFeat) {
                temp++;
            }
        }
    }
}


// Function to input the instructions according to the count feature
var countItem;

function countTask() {
    if (countOrder[0][0] === "color") {
        if (countOrder[0][1] === "b") {
            countItem = "blue item";
        } else {
            countItem = "green item";
        }
    } else {
        if (countOrder[0][1] === "hat") {
            countItem = "hat";
        } else {
            countItem = "chair";
        }
    }

    $("#instructCount").html('<ul style="list-style-type:none">' +
        '<li>Your second task is to keep count of how many trials include an image of a <strong>' + countItem + '</strong></li>' +
        '<li>Periodically, you will be asked to report the number you have counted. After you have given a response, you will start counting again, <strong>starting from zero</strong> </li></ul>'
    );
    $("#quizInstruct1").html("How many trials have included a " + countItem + "?");
    $("#quizInstruct2").html("Restart your count from zero, and count the number of trials that contain a " + countItem + " .</p> <h4> Press 'Continue' to begin the next trial</h4><p>");
    $("#reminder").html("*** Reminder: Count the number of trials that contain a " + countItem + " ***");


}


//////////////////////////////////////////////////////////////////////


////////////// Trial Events ////////////////////////////////////////////

/* set the URL for any images */
var imgURL = "stim_contexts/";



/* set trial intervals in milliseconds */
var blankLength = 400;
var fixateLength = 200;
var feedbackLength = 1000;

/* initialize necessary variables */
var response; /* temporary response container */
var accuracy; /* temporary accuracy container */
var data = [
    []
]; /* empty data array */
var expBegin = "NA"; /* first stimulus timestamp */
var expEnd = "NA"; /* end of experiment timestamp */

var trialCount = 0; /* trial counter / keeps track of which trial the subject is on */
var keytest = 0; /* enables and disables keypress effects / only allows responses when keytest == 1 */
var subject = new Date().getTime(); /* creates timestamp for unique subject identifier */

/* trial events in chronological order */

/* 1. blank screen */
function blank() {
    keytest = 0; /* disable keypresses */
    $("#quizDisplay").hide();
    $(".targetDisplay").hide(); /* hide display */
    $(".feedbackDisplay").hide();

    /* run function fixate after "blankLength" milliseconds */
    setTimeout(fixate, blankLength);
}

/* 2. fixation */
function fixate() {

    /* display fixation cross */
    $(".targetDisplay").hide();
    $(".targetDisplay").html("+");
    $(".targetDisplay").css("color", "black");
    $(".targetDisplay").show();

    /* run function blank2 after "fixateLength" milliseconds */
    setTimeout(blank2, fixateLength);
}

/* 3. blank screen */
function blank2() {
    $(".targetDisplay").hide(); /* hide display */
    /* run function blank2 after "blankLength" milliseconds */
    setTimeout(trial, blankLength);
}

/* 4. stim presentation / present until response */
function trial() {
    /* update counter display */
    $(".countDisplay").html((trialCount + 1) + " /" + trialArray.length + " trials");

    if (trialArray[trialCount].target === "quiz") {
        $("#quizDisplay").show();
        $('#targetDisplay').hide();
        document.getElementById("quizResponse").focus();
        time1 = new Date().getTime();
    } else {
        /* allow responses */
        keytest = 1;

        /* present target stim */
        $(".targetDisplay").hide();
        $(".targetDisplay").html('<img src="' + imgURL + trialArray[trialCount].flanker + '.png"' + '/>' + '<img src="stim_contexts/' + trialArray[trialCount].identity + trialArray[trialCount].color + '.jpg"' + 'style = "width:250px; height: 250px"/>');
        $(".targetDisplay").show();

        /* get timestamp for stim presentation */
        time1 = new Date().getTime();
    }

}

/* collect response on keypress / ends trial / only when keytest == 1 */
$(document).keypress(function(event) {
    if (keytest == 1) {
        keytest = 0; /* disable keypresses */
        time2 = new Date().getTime(); /* get timestamp for response */

        /* collect response / force to lower case */
        response = String.fromCharCode(event.which);
        response = response.toLowerCase();

        /* determine accuracy */
        if (response == trialArray[trialCount].cResponse) {
            accuracy = 1;
        } else {
            accuracy = 0;
        }

        /* run data collection function that adds trial info to data array */
        dataCollection();

        /* send data to parent window */
        sendData();

        /* display feedback */
        $(".targetDisplay").hide();
        $('.targetDisplay').css("color", "black");

        if (response == trialArray[trialCount].cResponse) {
            /* When response is correct... */
            $('.feedbackDisplay').html("correct");
            $(".feedbackDisplay").show();
        } else {
            /* when response is incorrect... */
            $('.feedbackDisplay').html("incorrect");
            $(".feedbackDisplay").show();
        }

        /* end current trial */
        endTrial();

    }
});

/* triggers end of trial */
function endTrial() {
    /* go to instructionDisplay2 if halfway, continue to next trial if trialCount < total number of trials, otherwise go to exit page */
    if (trialCount === (((trialArray.length) / 2) - 1)) {
        trialCount++; /* increase trial counter by one */
        if (countOrder[1][0] === "color") {
            if (countOrder[1][1] === "b") {
                countItem = "blue item";
            } else {
                countItem = "green item";
            }
        } else {
            if (countOrder[1][1] === "hat") {
                countItem = "hat";
            } else {
                countItem = "chair";
            }
        }

        $("#instructCount2").html(
            '<p><strong>However</strong>, your second task will now be to keep count of how many trials contain an image of a <strong>' + countItem + '</strong></p>' +
            '<p>Periodically you will be asked to report the number you have counted, and to restart from zero. </p>'
        );
        $("#quizInstruct1").html("How many trials trials have you counted that contain a " + countItem + "?");
        $("#quizInstruct2").html("Restart from zero, and count the number of trials that contain a " + countItem + " . Press 'Continue' to begin the next trial");
        $("#reminder").html("*** Reminder: Count the number of trials that contain a " + countItem + " ***");

        $(".targetDisplay").hide();
        $(".feedbackDisplay").hide();
        $("#quizDisplay").hide();
        $(".instructionDisplay2").show();

        /* continue to next trial if trialCount < total number of trials, otherwise go to exit page */
    } else if (trialCount != trialArray.length - 1) {
        trialCount++; /* increase trial counter by one */
        $("#quizDisplay").hide();
        /* automatically starts next trial / run function blank after "feedbackLength" milliseconds */
        setTimeout(blank, feedbackLength);
    } else {
        /* END OF EXPERIMENT */
        /* send up-to-date data set to parent window */
        expEnd = new Date().getTime();
        sendData();

        /* make sure debrief is shown and close pop-up */
        window.opener.endofExp();
        window.opener.scrollTo(500, 0);
        window.close();

    }

}

// Sets up the instruction pop-up //
function setPopUp (){
  // Get the modal
  var modal = document.getElementById('popUpInstructions');

  // Get the button that opens the modal
  var btn = document.getElementById("showInstructions");

  // Get the <span> element that closes the modal
  var span = document.getElementsByClassName("close")[0];

  // When the user clicks the button, open the modal
  btn.onclick = function() {
      $('#popUpText').html($('#textInstructions').html());
      modal.style.display = "block";
  };

  // When the user clicks on <span> (x), close the modal
  span.onclick = function() {
      modal.style.display = "none";
  };

  // When the user clicks anywhere outside of the modal, close it
  window.onclick = function(event) {
      if (event.target == modal) {
          modal.style.display = "none";
      }
  };
}
function sendData(){
  $("#RTs", opener.window.document).val("," + expBegin + "," + expEnd + "," + deminfo + "," + countOrder + ":" + data.join(":") );
}


////////////// Set up initial display & button functions //////////////////////////////////////
/* These functions need to run after document has loaded
 * So you must run this function in the header using document.ready or in the body of html (where it is now)
 */
function initiateDisplay() {
    /* initial display */
    $(".getData").hide();
    $("#imgPlaceHolder").hide();
    $(".targetDisplay").hide();
    $(".top").hide();
    $(".exitDisplay").hide();
    $(".fixateDisplay").hide();
    $(".instructionDisplay").hide();
    $(".instructionDisplay2").hide();
    $(".loading").hide();
    $("#errorLoading").hide();
    $("#quizDisplay").hide();
    $(".demographics").show();
    // Set preloading (imgURL, imageList)
    preLoad.addURL(imgURL, imageList);
    preLoad.loadImages(".loading", ".instructionDisplay", "#perc");
    // Set instructions per counting feature
    countTask();
    // Set up pop-up modal //
    setPopUp();




    ///////////Check if Amazon Turker is in Preview Mode//////////////////
    if (window.opener.previewit == 1) {
        $("#previewMode").show();
    } else {
        $("#previewMode").hide();
        window.opener.endofExp();
        window.opener.scrollTo(500, 0);
    }


    /* button functions */

    // Clicking Submit Demographics Button
    $("#endDem").click(function() {
        deminfo = ($("#Country").val() + "," +
            $("#Sex:checked").val() + "," +
            $("#Age").val() + "," +
            $("#Hand:checked").val() + "," +
            $("#Vision:checked").val() + "," +
            $("#English:checked").val() + "," +
            BrowserInfo[0] + ",");

        $(".demographics").hide();
        //$(".loading").show();
        preLoad.manualCheck(".instructionDisplay", ".loading");
        // check for completed image pre-load / inputs: show div, hide div, count 0-100% in "perc" div, error loading div
        //preLoad.loadImages();
    });



    /* end instructions and begin experiment */
    $("#beginExp").click(function() {
        expBegin = new Date().getTime();
        $(".instructionDisplay").hide();
        $(".countDisplay").html(trialCount + " / " + trialArray.length + " trials");
        $(".top").show();
        blank();
    });


    /* end instructions and begin experiment */
    $("#beginExp2").click(function() {
        $(".instructionDisplay2").hide();
        blank();
    });

    /* download data file */
    $("#downloadCSV").click(function() {
        /* adds header row to beginning of array and saves csv file */
        data.unshift(["subject", "trial", "word", "color", "correct-response", "congruency", "reaction-time", "response", "accuracy"]);
        exportToCsv('Stroop - ' + subject + '.csv', data);
    });


    //Continue after quiz trial //
    $("#quizButton").click(function() {
        time2 = new Date().getTime(); /* get timestamp for response */
        /* collect response / force to lower case */
        response = document.getElementById("quizResponse").value;

        /* reset form */
        document.getElementById("quiz").reset();

        /* determine accuracy */
        if (response == trialArray[trialCount].cResponse) {
            accuracy = 1;
        } else {
            accuracy = 0;
        }

        /* run data collection function that adds trial info to data array */
        dataCollection();
        data[trialCount].push(Number(data[trialCount][5]) - Number(data[trialCount][3]));
        data[trialCount].push("filler", "filler", "filler", "filler", "filler", "filler", "filler", "filler");


        /* send data to parent window */
        sendData();
        /* end current trial */
        endTrial();

    });



    /* Disable the backspace key / can add other keys if necessary */
    $(function() {
        /*
         * this swallows backspace keys on any non-input element.
         * stops backspace -> back
         */
        var rx = /INPUT|SELECT|TEXTAREA/i;

        $(document).bind("keydown keypress", function(e) {
            if (e.which == 8) { // 8 == backspace
                if (!rx.test(e.target.tagName) || e.target.disabled || e.target.readOnly) {
                    e.preventDefault();
                }
            }
        });
    });
}




/////////////////// Generic Functions /////////////////////////////////



/* compute average of array */
function calcAVG(array) {
    var total = 0;
    for (var i = 0; i < array.length; i++) {
        total += array[i];
    }
    var avg = total / array.length;
    return (avg);
}



/* shuffle an array */
function shuffle(array) {
    var tmp, current, top = array.length;
    if (top)
        while (--top) {
            current = Math.floor(Math.random() * (top + 1));
            tmp = array[current];
            array[current] = array[top];
            array[top] = tmp;
        }

    return array;
}

/* multiply an array */

//copies the array and stacks them into a single array
function multiplyArray(input, ntimes) {
    var temparray = new Array(new Array(''));
    iii = -1;
    for (i = 0; i <= ntimes - 1; i++) {
        for (ii = 0; ii <= input.length - 1; ii++) {
            iii++;
            temparray[iii] = input[ii].concat();
        }
    }
    return temparray;
}

/* Data Collection / generic function that puts all the trial information into data array*/
/* Requires global variables: subject, trialCount, trialArray, time1, time2, response, and accuracy
/*
e.g.,
data [
  subject identifier,
  trial number,
  ...
  all trialArray properties,
  ...
  reaction time (time2-time1),
  response,
  accuracy
]
*/

function dataCollection() {
    /* creates new row in data array for current trial that includes subject identifier and trial number */
    data[trialCount] = [subject, (trialCount + 1)];

    /* loops through object properties and pushes all property values into data array */
    for (var propName in trialArray[trialCount]) {
        data[trialCount].push(trialArray[trialCount][propName]);
    }

    /* pushes the time stamps and response into data array */
    data[trialCount].push(
        time1,
        time2,
        response,
        accuracy
    );
}



/* save array as csv file */
function exportToCsv(filename, rows) {
    var processRow = function(row) {
        var finalVal = '';
        for (var j = 0; j < row.length; j++) {
            var innerValue = row[j] === null ? '' : row[j].toString();
            if (row[j] instanceof Date) {
                innerValue = row[j].toLocaleString();
            }
            var result = innerValue.replace(/"/g, '""');
            if (result.search(/("|,|\n)/g) >= 0)
                result = '"' + result + '"';
            if (j > 0)
                finalVal += ',';
            finalVal += result;
        }
        return finalVal + '\n';
    };

    var csvFile = '';
    for (var i = 0; i < rows.length; i++) {
        csvFile += processRow(rows[i]);
    }

    var blob = new Blob([csvFile], {
        type: 'text/csv;charset=utf-8;'
    });
    if (navigator.msSaveBlob) { // IE 10+
        navigator.msSaveBlob(blob, filename);
    } else {
        var link = document.createElement("a");
        if (link.download !== undefined) { // feature detection
            // Browsers that support HTML5 download attribute
            var url = URL.createObjectURL(blob);
            link.setAttribute("href", url);
            link.setAttribute("download", filename);
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        }
    }
}


/* Example:

exportToCsv('export.csv', [
	['name','description'],
  ['david','123'],
  ['jona','""'],
  ['a','b'],
]);

*/
