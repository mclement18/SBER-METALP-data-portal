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
