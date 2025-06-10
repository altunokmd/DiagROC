module.exports = {

    view_updated(ui) {
        ui.dismin_comp.setEnabled(false);
        ui.dismax_comp.setEnabled(false);
        this.calc_disagreement(ui)
    },

    prop1_comp_changed(ui) {
        this.calc_disagreement(ui)
    },
    prop2_comp_changed(ui) {
        this.calc_disagreement(ui)
    },

    calc_disagreement(ui) {
        const p1 = ui.prop1_comp.value() / 100
        const p2 = ui.prop2_comp.value() / 100
        ui.dismin_comp.setValue(Math.round(Math.abs(p1 - p2) * 100))
        ui.dismax_comp.setValue(Math.round((p1 * (1 - p2) + p2 * (1 - p1)) * 100))
    }
}