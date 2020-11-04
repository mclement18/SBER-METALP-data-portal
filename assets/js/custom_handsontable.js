const CustomHandsontable = {};

// Create a time validator
CustomHandsontable.timeFormatValidator = function (value, callback) {
    callback(/^\d{2}:\d{2}:\d{2}$/.test(value));
};


// Create a time renderer
CustomHandsontable.dateRenderer = function (hotInstance, td, row, column, prop, value, cellProperties) {
    // Optionally include `BaseRenderer` which is responsible for adding/removing CSS classes to/from the table cells.
    Handsontable.renderers.BaseRenderer.apply(this, arguments);

    // Time renderer
    // If time format is 'DD/MM/YYYY' convert to 'YYYY-MM-DD'
    if (/^\d{2}\/\d{2}\/\d{4}$/.test(value)) {
        value = value.split('/').reverse().join('-');
    }
    // Set value
    td.textContent = value;
};

// Set table input
CustomHandsontable.setInputId = function (tableDOM, attrName, inputId) {
    tableDOM.dataset[attrName] = inputId;
};

// Create an onChange hook callback
CustomHandsontable.onChangeCallback = function (changes) {
    if (changes) {
        const inputId = this.rootElement.dataset.onChangeId;
        const message = [];
        changes.forEach(([row, col, oldValue, newValue]) => {
            if (oldValue !== newValue) {

                message.push({
                    row: row + 1,
                    column: col + 1,
                    value: newValue
                });

                this.setCellMeta(row, col, 'className', 'changed');
                this.render();
            }
        });

        Shiny.setInputValue(inputId, JSON.stringify(message), {priority: "event"});
    }
};

// Create an onChange hook
CustomHandsontable.onChange = function (tableDOM, hot, inputId) {
    // Set onChangeInputId
    this.setInputId(tableDOM, 'onChangeId', inputId);
    // Remove hook in case of multiple rendering from shiny
    hot.removeHook('afterChange', CustomHandsontable.onChangeCallback);
    // Add hook
    hot.addHook('afterChange', CustomHandsontable.onChangeCallback);
};

// Create afterSelectionEnd hook callback for row deletion purpose
CustomHandsontable.afterSelectionCallback = function (rowStart, colStart, rowEnd, colEnd, selectionLayerLevel) {
    const inputId = this.rootElement.dataset.afterSelectionId;
    const message = {
        start: null,
        end: null
    };

    if (rowStart === rowEnd) {
        message.start = rowStart;
    } else if (rowStart < rowEnd) {
        message.start = rowStart;
        message.end = rowEnd;
    } else if (rowStart > rowEnd) {
        message.start = rowEnd;
        message.end = rowStart;
    }

    Shiny.setInputValue(inputId, message, {priority: "event"});
};

// Create afterDeselect hook callback to remove the selection
CustomHandsontable.afterDeselectCallback = function () {
    const inputId = this.rootElement.dataset.afterSelectionId;
    const message = {
        start: null,
        end: null
    };
    setTimeout(() => {
        Shiny.setInputValue(inputId, message, {priority: "event"});
    }, 1000);
};

// Create afterSelectionEnd hook for row deletion purpose
CustomHandsontable.afterSelection = function (tableDOM, hot, inputId) {
    // Set onChangeInputId
    this.setInputId(tableDOM, 'afterSelectionId', inputId);
    // Remove hooks in case of multiple rendering from shiny
    hot.removeHook('afterSelectionEnd', CustomHandsontable.afterSelectionCallback);
    hot.removeHook('afterDeselect', CustomHandsontable.afterDeselectCallback);
    // Add hooks
    hot.addHook('afterSelectionEnd', CustomHandsontable.afterSelectionCallback);
    hot.addHook('afterDeselect', CustomHandsontable.afterDeselectCallback);
};
