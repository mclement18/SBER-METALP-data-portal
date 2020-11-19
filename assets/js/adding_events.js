// Add all events on when dom finished to load

document.addEventListener('DOMContentLoaded', () => {
    Utils.addScrollTopToFooterNav();
    Utils.addBannerWidthCorrection();
    PlotDownload.addEventListeners();
});
