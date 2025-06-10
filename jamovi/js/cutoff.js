module.exports = {

    view_updated(ui) {
        this._checkMeasureType(ui, "view");
        this.findChanges("cutoff", ui.dep.value(), true, FormatDef.term)
    },

    dep_changed(ui) {
        const c = this.findChanges("cutoff", ui.dep.value(), true, FormatDef.term)
        if (c.hasChanged) this._checkMeasureType(ui, "changed");
    },

    cutoffs_listItemAdded(ui) {
        let c = ui.cutoffs.value()
        for(let i in c) {
            if (c[i] == null) c[i] = { cutoff: null, direction: ">=" }
        }
        ui.cutoffs.setValue(c)
    },

    cutoffs_listItemRemoved(ui) {
        let c = ui.cutoffs.value()
        let v = ui.dep.value()
        if (!v) return;
        if (c.length > 0) return;
        this.requestData("column", {
            columnName: v,
            properties: ["measureType", "dataType", "levels"]
        }).then(info => {
            if(!this._isBinary(info)) ui.cutoffs.setValue([{ cutoff: null, direction: ">=" }])
        })
    },

    _checkMeasureType(ui, source) {
        const variable = ui.dep.value();

        if (!variable) {
            this._updateCutoffUI(ui, "init", source);
            return;
        }

        this.requestData("column", {
            columnName: variable,
            properties: ["measureType", "dataType", "levels"]
        }).then(info => {
            this._updateCutoffUI(ui, info, source);
        });
    },

    _updateCutoffUI(ui, info, source) {
        const isBinary = info == "init" ? true : this._isBinary(info);

        ui.cutoffs.$el.css("opacity", isBinary ? "0.5" : "1");
        ui.cutoffs.$addButton.css("display", isBinary ? "none" : "grid");

        if(source == "changed" || info == "init") {
            const defaultCutoffs = isBinary ? [] : [{ cutoff: null, direction: ">=" }];
            ui.cutoffs.setValue(defaultCutoffs);
        }

        const enableTestPos = isBinary && info != "init";
        ui.testpos.setEnabled(enableTestPos);
    },

    _isBinary(info) {
        const isNotContinuous = info.measureType !== "continuous";
        const isTextOrTwoLevels = info.dataType === "text" || (Array.isArray(info.levels) && info.levels.length <= 2);
        return isNotContinuous && isTextOrTwoLevels;
    }
};
