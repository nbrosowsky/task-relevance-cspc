/////// PreLoad Images ///////
//Send image URLs to preload array
var preLoad = {
    nLoaded: 0, //number of images checked for pre-load
    yourImages: [], //image array
    preImages: new Array(),
    check: new Array(),
    loading: {},
    finished: {},
    progress: {},



    addURL: function(imgURL, imageList) {
        //create URL array from imageList
        for (var i = 0; i <= imageList.length - 1; ++i) {
            preLoad.yourImages[i] = imgURL + imageList[i];
        }
    },

    loadImages: function(loadingDiv, completeDiv, progressDiv) {

        this.loading = loadingDiv;
        this.finished = completeDiv;
        this.progress = progressDiv;

        if (this.nLoaded < preLoad.yourImages.length) {
            this.preImages[this.nLoaded] = new Image();
            this.preImages[this.nLoaded].onload = function() {
                preLoad.checkLoad();
            };

            this.preImages[this.nLoaded].onerror = function() {
                preLoad.checkLoad();
            };


            this.preImages[this.nLoaded].src = this.yourImages[this.nLoaded];
            //  document.getElementById("imgPlaceHolder").appendChild(this.preImages[this.index]);
        }
    },

    checkLoad: function() {
        this.nLoaded++;
        //  preLoad.index = preLoad.index + 1;
        $(this.progress).html(Math.round((preLoad.nLoaded / preLoad.yourImages.length) * 100) + "%");
        if (preLoad.nLoaded < preLoad.yourImages.length) {
            preLoad.loadImages(this.loading, this.finished, this.progress);
        }
        if (preLoad.nLoaded == preLoad.yourImages.length && ($(this.loading).css('display') != 'none')) {
            $(this.loading).hide();
            $(this.finished).show();
        }
    },

    manualCheck: function(complete, incomplete) {
        if (preLoad.nLoaded == preLoad.yourImages.length) {
            $(complete).show();
        } else {
            $(incomplete).show();
        }
    }

};
