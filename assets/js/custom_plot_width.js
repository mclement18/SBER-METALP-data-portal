// Module to set the plots width
// Linked to the sidebarInputLayout module defined in './modules/visualisation_tab/sidebar_input_layout.R'
const CustomPlotWidth = {};

CustomPlotWidth.setNewWidth = function(id, width) {
    // Get the main plots container
    const container = document.querySelector(`#${CSS.escape(id)}`);

    if (container) {
        // Iterate over its childrens (i.e. row with possible multiple plots)
        for (let i = 0; i < container.children.length; i++) {
            const plotRow = container.children[i];
            // Iterate over the plots (which have a container for the spinner)
            for (let j = 0; j < plotRow.children.length; j++) {
                const plotContainer = plotRow.children[j];
                // Set width of the plot container
                plotContainer.style.width = width;

                // Get the shinz plot output
                const plotOutput = plotContainer.querySelector('.shiny-plot-output');
                if (plotOutput) {
                    // Set width of the Shiny plot output
                    plotContainer.querySelector('.shiny-plot-output').style.width = width;
                }
            }
        }
        
        // Send a resize window event to shiny to replay and resize the plots
        window.dispatchEvent(new Event('resize'));
    }
};

CustomPlotWidth.setNewWidthCallback = function(message) {
    const {containerId, width} = message;
    if (typeof width === 'string') {
        this.setNewWidth(containerId, width);
    }
};
