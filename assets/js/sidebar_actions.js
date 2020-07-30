// Module to show and hide the shiny sidebar from a sidebar panel
// Linked to the sidebarInputLayout module defined in './modules/visualisation_tab/sidebar_input_layout.R'
const Sidebar = {};

// Get sidebar element with enclosing div used for layout
Sidebar.getSidebar = function(id) {
    return document.querySelector(`#${id}`).parentElement;
};

// Get main panel
Sidebar.getMainPanel = function(id) {
    return document.querySelector(`#${id}`);
}

// Hide sidebar
Sidebar.hide = function(sidebarId, mainPanelId) {
    // Only if it not already hidden
    if (this.getSidebar(sidebarId).style.display !== 'none' && this.getMainPanel(mainPanelId).classList.contains('col-sm-9')) {
        // Add inline style: 'display: non;'
        this.getSidebar(sidebarId).style.display = 'none';
        // Change main panel class name to make it span over the full space
        this.getMainPanel(mainPanelId).classList.replace('col-sm-9', 'col-sm-12');
        // Send a resize window event to shiny to replay and resize the plots
        window.dispatchEvent(new Event('resize'));
    }
};

// Show sidebar
Sidebar.show = function(sidebarId, mainPanelId) {
    // Only if it not already visible
    if (this.getSidebar(sidebarId).style.display === 'none' && this.getMainPanel(mainPanelId).classList.contains('col-sm-12')) {
        // Remove inline style: 'display: non;'
        this.getSidebar(sidebarId).style.display = '';
        // Change main panel class name to place it next to the sidebar
        this.getMainPanel(mainPanelId).classList.replace('col-sm-12', 'col-sm-9');
        // Send a resize window event to shiny to replay and resize the plots
        window.dispatchEvent(new Event('resize'));
    }
};

// Toggle sidebar status between visible and hidden
// Accept a message from a shiny event
Sidebar.toggle = function(message) {
    const {sidebarId, mainPanelId, show} = message;

    // If the boolean 'show' is true, show the sidebar
    // Else, hide it
    if (show) {
        this.show(sidebarId, mainPanelId);
    } else {
        this.hide(sidebarId, mainPanelId);
    }
};
