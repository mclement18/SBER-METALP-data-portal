const PlotDownload = {};

PlotDownload.buildDownloadIcon = function(link) {
    const icon = document.createElement('i');
    icon.className = 'far fa-save';

    const id = `download-icon-${Utils.randomHex()}`;

    const aTag = document.createElement('a');
    aTag.id = id;
    aTag.href = link;
    aTag.download = 'metalp_plot.png';
    aTag.className = 'plot-download';
    aTag.appendChild(icon);

    return [id, aTag];
};

PlotDownload.showDowloadIcon = function(image) {
    // Get parent container
    const imageContainer = image.parentElement;
    // Set class
    imageContainer.classList.add('has-download-icon');
    // Create download icon and add it
    const [id, dowloadIcon] = this.buildDownloadIcon(image.src);
    imageContainer.appendChild(dowloadIcon);

    return id;
};

PlotDownload.removeDowloadIcon = function(image, iconId) {
    // Get parent container
    const imageContainer = image.parentElement;
    if (iconId) {
        // Remove icon
        const downloadIcon = document.querySelector(`#${iconId}`);
        if (downloadIcon) {
            imageContainer.removeChild(downloadIcon);
        }
    } else {
        // Remove all icons
        imageContainer.querySelectorAll('.plot-download').forEach(icon => {
            imageContainer.removeChild(icon);
        });
    }

    // Remove class if no icon left
    this.removeContainerClass(imageContainer);
};

PlotDownload.removeContainerClass = function(container) {
    if (container.querySelector('.plot-download') === null) {
        container.classList.remove('has-download-icon');
    }
};

PlotDownload.addEventListeners = function() {
    const visuTabContent = document.querySelector('div[data-value="visuTab"]').querySelector('.tab-content');
    visuTabContent.addEventListener('mouseover', e => {
        const target = e.target;

        if (target.tagName === 'IMG') {
            PlotDownload.removeDowloadIcon(target);
            const id = PlotDownload.showDowloadIcon(target);

            setTimeout(() => {
                PlotDownload.removeDowloadIcon(target, id);
            }, 2000);
        }
    });
};
