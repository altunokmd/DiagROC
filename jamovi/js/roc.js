module.exports = {

    enableCompareRoc: function(ui) {
        const depsCount = ui.deps.value().length
        ui.compareRoc.setEnabled(depsCount > 1)
    }

}
