const PointHoverWidget = {};

PointHoverWidget.getPlot = function(id) {
    return document.querySelector(`#${CSS.escape(id)}`);
};

PointHoverWidget.getPlotWidget = function(id) {
    return this.getPlot(id).querySelector('.point-hover-widget');
};

PointHoverWidget.buildValueInfo = function(axis, name, value) {
    const valueElement = document.createElement('span');
    valueElement.className = `${axis}__value value`;
    valueElement.textContent = value;

    const nameElement = document.createElement('span');
    nameElement.className = `${axis}__name name`;
    nameElement.textContent = `${name}:`;

    const info = document.createElement('span');
    info.className = `${axis} point-info`;
    info.appendChild(nameElement);
    info.appendChild(valueElement);

    return info;
};

PointHoverWidget.addWidget = function(plotId, pointInfo, hoverInfo) {
    const bubble = document.createElement('div');
    bubble.className = 'bubble';
    bubble.appendChild(this.buildValueInfo('x', hoverInfo.mapping.x, pointInfo[hoverInfo.mapping.x]));
    bubble.appendChild(this.buildValueInfo('y', hoverInfo.mapping.y, pointInfo[hoverInfo.mapping.y]));
    
    const widget = document.createElement('div');
    widget.className = 'point-hover-widget';
    widget.style.left = `${hoverInfo.coords_img.x - 10}px`;
    widget.style.bottom = `${400 - hoverInfo.coords_img.y + 10}px`;
    widget.appendChild(bubble);

    this.getPlot(plotId).appendChild(widget);
};

PointHoverWidget.removeWidget = function(plotId) {
    if (this.getPlotWidget(plotId)) {
        this.getPlot(plotId).removeChild(this.getPlotWidget(plotId));
    }
};

PointHoverWidget.inputEqualCurrentWidget = function(currentWidget, x, y) {
    return (currentWidget.querySelector('.x__value').textContent === toString(x)
            &&
            currentWidget.querySelector('.y__value').textContent === toString(y));
};

PointHoverWidget.needUpdate = function(plotId, pointInfo, mapping) {
    const currentWidget = this.getPlotWidget(plotId);

    return (currentWidget !== null && this.inputEqualCurrentWidget(currentWidget, pointInfo[mapping.x], pointInfo[mapping.y]))
    ? false : true;
};

PointHoverWidget.addWidgetCallback = function(message) {
    const {pointInfo, hoverInfo, plotId} = message;

    if (this.needUpdate(plotId, pointInfo, hoverInfo.mapping)) {
        console.log('adding');
        this.removeWidget(plotId);
        this.addWidget(plotId, pointInfo, hoverInfo);
    }
};

PointHoverWidget.removeWidgetCallback = function(message) {
    this.removeWidget(message.plotId);
};
