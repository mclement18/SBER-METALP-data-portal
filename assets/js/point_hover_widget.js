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

PointHoverWidget.addWidget = function(plotId, pointInfo, mapping, coords_img, x_y_labels) {
    const bubble = document.createElement('div');
    bubble.className = 'bubble';
    bubble.appendChild(this.buildValueInfo('x', x_y_labels.x, pointInfo[mapping.x]));
    bubble.appendChild(this.buildValueInfo('y', x_y_labels.y, pointInfo[mapping.y]));
    bubble.appendChild(this.buildValueInfo('site', 'Station', pointInfo.Site_ID));
    
    const widget = document.createElement('div');
    widget.className = 'point-hover-widget';
    widget.style.left = `${coords_img.x - 25}px`;
    widget.style.bottom = `${400 - coords_img.y + 10}px`;
    widget.appendChild(bubble);

    this.getPlot(plotId).appendChild(widget);
};

PointHoverWidget.removeWidget = function(plotId) {
    if (this.getPlotWidget(plotId)) {
        this.getPlot(plotId).removeChild(this.getPlotWidget(plotId));
    }
};

PointHoverWidget.inputEqualCurrentWidget = function(currentWidget, x, y) {
    return (currentWidget.querySelector('.x__value').textContent === x
            &&
            currentWidget.querySelector('.y__value').textContent === y.toString());
};

PointHoverWidget.needUpdate = function(plotId, pointInfo, mapping) {
    const currentWidget = this.getPlotWidget(plotId);

    return (currentWidget !== null && this.inputEqualCurrentWidget(currentWidget, pointInfo[mapping.x], pointInfo[mapping.y]))
    ? false : true;
};

PointHoverWidget.addWidgetCallback = function(message) {
    const {pointInfo, mapping, coords_img, x_y_labels, plotId} = message;

    if (this.needUpdate(plotId, pointInfo, mapping)) {
        this.removeWidget(plotId);
        this.addWidget(plotId, pointInfo, mapping, coords_img, x_y_labels);
    }
};

PointHoverWidget.removeWidgetCallback = function(message) {
    this.removeWidget(message.plotId);
};
