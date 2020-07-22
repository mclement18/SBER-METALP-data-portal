const Sidebar = {};

Sidebar.getSidebar = function(id) {
    return document.querySelector(`#${id}`).parentElement;
};

Sidebar.getMainPanel = function(id) {
    return document.querySelector(`#${id}`);
}

Sidebar.hide = function(sidebarId, mainPanelId) {
    if (this.getSidebar(sidebarId).style.display !== 'none' && this.getMainPanel(mainPanelId).classList.contains('col-sm-9')) {
        this.getSidebar(sidebarId).style.display = 'none';
        this.getMainPanel(mainPanelId).classList.replace('col-sm-9', 'col-sm-12');
        window.dispatchEvent(new Event('resize'));
    }
};

Sidebar.show = function(sidebarId, mainPanelId) {
    if (this.getSidebar(sidebarId).style.display === 'none' && this.getMainPanel(mainPanelId).classList.contains('col-sm-12')) {
        this.getSidebar(sidebarId).style.display = '';
        this.getMainPanel(mainPanelId).classList.replace('col-sm-12', 'col-sm-9');
        window.dispatchEvent(new Event('resize'));
    }
};

Sidebar.toggle = function(message) {
    const {sidebarId, mainPanelId, show} = message;

    if (show) {
        this.show(sidebarId, mainPanelId);
    } else {
        this.hide(sidebarId, mainPanelId);
    }
};
