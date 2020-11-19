// Add custom shiny events
Shiny.addCustomMessageHandler('sidebarToggle', Sidebar.toggle.bind(Sidebar));
Shiny.addCustomMessageHandler('addHoverWidget', PointHoverWidget.addWidgetCallback.bind(PointHoverWidget));
Shiny.addCustomMessageHandler('removeHoverWidget', PointHoverWidget.removeWidgetCallback.bind(PointHoverWidget));
Shiny.addCustomMessageHandler('toggleDownloadButton', DownloadButtonState.toggleCallback.bind(DownloadButtonState));
Shiny.addCustomMessageHandler('setPlotWidth', CustomPlotWidth.setNewWidthCallback.bind(CustomPlotWidth));
