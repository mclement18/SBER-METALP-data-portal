Shiny.addCustomMessageHandler('sidebarToggle', Sidebar.toggle.bind(Sidebar));
Shiny.addCustomMessageHandler('addHoverWidget', PointHoverWidget.addWidgetCallback.bind(PointHoverWidget));
Shiny.addCustomMessageHandler('removeHoverWidget', PointHoverWidget.removeWidgetCallback.bind(PointHoverWidget));
