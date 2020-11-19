const PlotDownload = {};

PlotDownload.buildDownloadIcon = function(link) {
    const icon = document.createElement('i');
    icon.className = 'far fa-save';

    const aTag = document.createElement('a');
    aTag.href = link;
    aTag.download = 'metalp_plot.png';
    aTag.className = 'plot-download';
    aTag.appendChild(icon);

    return aTag;
};

PlotDownload.showDowloadIcon = function(image) {
    // Get parent container
    const imageContainer = image.parentElement;
    // Set class
    imageContainer.classList.add('has-download-icon');
    // Create download icon and add it
    const dowloadIcon = this.buildDownloadIcon(image.src);
    imageContainer.appendChild(dowloadIcon);
};

PlotDownload.removeDowloadIcon = function(image) {
    // Get parent container
    const imageContainer = image.parentElement;
    // Remove icon and class
    imageContainer.removeChild(imageContainer.querySelector('.plot-download'));
    imageContainer.classList.remove('has-download-icon');
};

PlotDownload.addEventListeners = function() {
    const visuTabContent = document.querySelector('div[data-value="visuTab"]').querySelector('.tab-content');
    visuTabContent.addEventListener('mouseover', e => {
        const target = e.target;

        if (target.tagName === 'IMG') {
            PlotDownload.showDowloadIcon(target);

            setTimeout(() => {
                PlotDownload.removeDowloadIcon(target);
            }, 2000);
        }
    });
};
