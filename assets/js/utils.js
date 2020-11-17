const Utils = {};

Utils.getFooterNav = function () {
    return document.querySelector('#footer-nav');
};

Utils.scrollTopCallabck = function (e) {
    const target = e.target;
    if(target.tagName === 'A') {
        e.preventDefault();
        window.scrollTo(0, 0);
    }
};

Utils.addScrollTopToFooterNav = function () {
    this.getFooterNav().addEventListener('click', this.scrollTopCallabck);
};

Utils.setBannerWidth = function() {
    const scrollBarY = window.innerWidth - document.body.clientWidth;
    if (scrollBarY > 0) {
        document.querySelector('#banner')
        .style.width = `calc(100vw - ${scrollBarY / 2}px)`;
    } else {
        document.querySelector('#banner')
        .style.width = '';
    }
};

Utils.addBannerWidthCorrection = function() {
    this.setBannerWidth();
    window.addEventListener('resize', this.setBannerWidth);    
};
