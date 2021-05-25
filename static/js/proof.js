// **************
// DISPLAY CONFIG
// **************

const RULES = {
    'axiom': '<span class="italic">ax</span>',
    'tensor': '⊗',
    'par': '<span class="flip">&</span>',
    'with': '&',
    'plus_left': '⊕<sub>1</sub>',
    'plus_right': '⊕<sub>2</sub>',
    'one': '1',
    'bottom': '⊥',
    'top': '⊤',
    // rule zero does not exist
    'promotion': '!',
    'dereliction': '?<span class="italic">d</span>',
    'contraction': '?<span class="italic">c</span>',
    'weakening': '?<span class="italic">w</span>',
    'exchange': '<span class="italic">ech</span>',
    'cut': '<span class="italic">cut</span>',
    'unfold_litt': '<span class="italic">def</span>',
    'unfold_dual': '<span class="italic">def</span>'
};

const ABBREVIATIONS = {
    '"sequent":': '"s":',
    '"appliedRule":': '"ar":',
    '"ruleRequest":': '"rr":',
    '"premises":': '"p":',
    '"rule":': '"r":',
    '"formulaPosition":': '"fp":',
    '"type":': '"t":',
    '"value":': '"v":',
    '"value1":': '"v1":',
    '"value2":': '"v2":'
}

// *************
// PROOF DISPLAY
// *************

function initProof(proofAsJson, $container, options = {}) {
    $container.data('options', options);

    let $proofDiv = $('<div>', {'class': 'proof'});
    $container.append($proofDiv);
    createSubProof(proofAsJson, $proofDiv, options);

    if (options.notations) {
        options.notations.formulasAsString = options.notations.formulasAsString || [];
        options.notations.formulas = [];
        createNotationBar($container, options.notations.formulasAsString);
    }

    if (options.autoReverse) {
        createOption($container, options.autoReverse.value, 'Auto-reverse',function (autoReverse) {
            saveOption($container, 'autoReverse', autoReverse);

            if (autoReverse) {
                autoReverseContainer($container);
            }

            options.autoReverse.onToggle(autoReverse);
        }, options.autoReverse.dialog);

        if (options.autoReverse.value) {
            autoReverseContainer($container);
        }
    }

    if (options.cutMode) {
        createOption($container, options.cutMode.value, 'Cut mode',function (cutMode) {
            saveOption($container, 'cutMode', cutMode);
            toggleCutMode($container, cutMode);
            options.cutMode.onToggle(cutMode);
        }, options.cutMode.dialog);

        toggleCutMode($container, options.cutMode.value);
    }

    if (options.exportButtons) {
        createExportBar($container);
    }
}

function createSubProof(proofAsJson, $subProofDivContainer, options) {
    let $sequentTable = createSequentTable(proofAsJson.sequent, options);
    $subProofDivContainer.prepend($sequentTable);

    if (proofAsJson.appliedRule) {
        let permutationBeforeRule = getSequentIdentityPermutation(proofAsJson.sequent);

        if (proofAsJson.appliedRule.ruleRequest.rule === 'exchange') {
            permutationBeforeRule = {'hyp': [], 'cons': invertPermutation(proofAsJson.appliedRule.ruleRequest.permutation)};
            proofAsJson = proofAsJson.appliedRule.premises[0];
        }

        addPremises($sequentTable,
            permutationBeforeRule,
            proofAsJson.appliedRule.ruleRequest,
            proofAsJson.appliedRule.premises,
            options);
    } else if (options.checkProvability) {
        checkProvability($sequentTable);
    }
}

function createSequentTable(sequent, options) {
    let $sequentTable = $('<table>')
        .data('sequentWithoutPermutation', sequent);

    let $td = $('<td>');
    $td.append(createSequent(sequent, $sequentTable, options));
    $sequentTable.append($td);

    let $tagBox = $('<td>', {'class': 'tagBox'})
        .html('&nbsp;');
    $sequentTable.append($tagBox);

    return $sequentTable;
}

function removeSequentTable($sequentTable) {
    undoRule($sequentTable);
    let $div = $sequentTable.closest('div');
    $sequentTable.remove();

    return $div;
}

// *******
// OPTIONS
// *******

function createOption($container, isChecked, text, onToggle, dialog) {
    let $input = $('<input type="checkbox">');
    $input.prop('checked', isChecked);
    $input.on('change', function() {
        onToggle(this.checked);
    });

    let $optionBar = $('<div>', {'class': 'option-bar'})
        .append($('<span>', {'class': 'option-label'}).text(text))
        .append($('<span>', {'class': 'option-info', 'title': `Learn about ${text} option`})
            .text('ⓘ')
            .on('click', function () { $(`#${dialog}`).dialog('open'); }))
        .append($('<label>', {'class': 'switch'})
            .append($input)
            .append($('<span class="slider"></span>')));

    $container.append($optionBar);
}

function saveOption($container, optionName, optionValue) {
    let options = $container.data('options');
    options[optionName].value = optionValue;
    $container.data('options', options);
}

// **********
// APPLY RULE
// **********

function applyRule(ruleRequest, $sequentTable) {
    let $container = $sequentTable.closest('.proof-container');
    let options = $container.data('options');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentTable.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentTable);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    let notations = getNotations($container);

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ ruleRequest, sequent, notations })),
        success: function(data)
        {
            if (data.success === true) {
                clearSavedProof();
                cleanPedagogicMessage($container);
                let appliedRule = data['proof'].appliedRule;
                addPremises($sequentTable, permutationBeforeRule, appliedRule.ruleRequest, appliedRule.premises, options);

                if (!isProved($sequentTable) && options.autoReverse?.value) {
                    autoReverseSequentPremises($sequentTable);
                }
            } else {
                displayPedagogicError(data['errorMessage'], $container);
            }
        },
        error: onAjaxError
    });
}

function addPremises($sequentTable, permutationBeforeRule, ruleRequest, premises, options) {
    // Undo previously applied rule if any
    undoRule($sequentTable);

    // Save data
    $sequentTable
        .data('permutationBeforeRule', permutationBeforeRule)
        .data('ruleRequest', ruleRequest);

    // Add line
    let $td = $sequentTable.find('div.sequent').closest('td');
    let dashedLine = ruleRequest.rule === 'unfold_litt' || ruleRequest.rule === 'unfold_dual';
    $td.addClass(dashedLine ? 'dashed-line' : 'solid-line');

    // Add rule symbol
    let $ruleSymbol = $('<div>', {'class': `tag ${ruleRequest.rule}`}).html(RULES[ruleRequest.rule]);
    if (options.withInteraction) {
        $ruleSymbol.addClass('clickable');
        $ruleSymbol.on('click', function() {
            clearSavedProof();
            undoRule($sequentTable);
        })
    }
    $td.next('.tagBox').html($ruleSymbol);

    // Add premises
    if (premises.length === 0) {
        if (options.withInteraction) {
            markParentSequentsAsProved($sequentTable);
        }
    } else if (premises.length === 1) {
        createSubProof(premises[0], $sequentTable.parent(), options);
    } else {
        let $div = $('<div>');
        $div.insertBefore($sequentTable);
        $sequentTable.addClass('binary-rule');
        for (let premise of premises) {
            let $sibling = $('<div>', {'class': 'sibling'})
            $div.append($sibling);
            createSubProof(premise, $sibling, options)
        }
    }
}

function undoRule($sequentTable) {
    // Erase data
    $sequentTable
        .data('permutationBeforeRule', null)
        .data('ruleRequest', null)
        .data('proved', null);

    // Remove line
    let $td = $sequentTable.find('div.sequent').closest('td');
    $td.removeClass('solid-line');
    $td.removeClass('dashed-line');

    // Remove rule symbol
    $td.next('.tagBox').html('');

    // Remove premises
    $sequentTable.prevAll().each(function (i, e) {
        e.remove();
    });
    $sequentTable.removeClass('binary-rule');

    // Mark proof as incomplete
    let $container = $sequentTable.closest('.proof-container');
    markAsIncomplete($container);
}

// ***************
// PEDAGOGIC ERROR
// ***************

function displayPedagogicError(message, $container) {
    displayPedagogicMessage(message, $container, 'error');
}

function displayPedagogicInfo(message, $container) {
    displayPedagogicMessage(message, $container, 'info');
}

function displayPedagogicMessage(errorMessage, $container, className) {
    let $div = $container
        .children('div.pedagogic-message');
    if (!$div.length) {
        $div = $('<div>', {'class': 'pedagogic-message'})
            .addClass(className);
        $div.append($('<div>', {'class': 'message'}));
        let $close = $('<div>', {'class': 'close-button'});
        $close.html('✖');
        $close.on('click', function () { cleanPedagogicMessage($container); });
        $div.append($close);

        let $proofDiv = $container.children('div.proof');
        if ($proofDiv.length) {
            $div.insertAfter($proofDiv);
        } else {
            $container.append($div);
        }
    } else {
        $div.removeClass();
        $div.addClass('pedagogic-message');
        $div.addClass(className);
    }
    $div.children('div.message').text(errorMessage);
}

function cleanPedagogicMessage($container) {
    $container
        .children('div.pedagogic-message')
        .remove();
}

// ******************
// GET COMPLETE PROOF
// ******************

function getProofAsJson($container) {
    let $mainTable = $container
        .children('div.proof')
        .children('table')
        .last();

    return recGetProofAsJson($mainTable);
}

function recGetProofAsJson($sequentTable) {
    let sequentWithoutPermutation = $sequentTable.data('sequentWithoutPermutation');
    let ruleRequest = $sequentTable.data('ruleRequest') || null;
    let appliedRule = null;
    if (ruleRequest !== null) {
        let $prev = $sequentTable.prev();
        let premises = [];
        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                premises = [recGetProofAsJson($prev)];
            } else {
                $prev.children('div.sibling').each(function (i, sibling) {
                    premises.push(recGetProofAsJson($(sibling).children('table').last()));
                })
            }
        }
        appliedRule = { ruleRequest, premises };

        let permutationBeforeRule = $sequentTable.data('permutationBeforeRule');
        let displayPermutation = getSequentPermutation($sequentTable);
        if (!isIdentitySequentPermutation(permutationBeforeRule)
            || !isIdentitySequentPermutation(displayPermutation)) {
            let sequentWithPermutation = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

            // Permutation asked by API is from premise to conclusion, and we have the other way
            // We need to invert permutation
            let invertedPermutation = invertPermutation(permutationBeforeRule['cons']);

            // Display permutation asked by API is from premise to displayed conclusion
            let permutedDisplayPermutation = permute(invertedPermutation, displayPermutation['cons']);

            appliedRule = {
                ruleRequest: {
                    rule: 'exchange',
                    permutation: invertedPermutation,
                    displayPermutation: permutedDisplayPermutation
                },
                premises: [{sequent: sequentWithPermutation, appliedRule}]
            }
        }
    }

    return { sequent: sequentWithoutPermutation, appliedRule };
}

function isIdentitySequentPermutation(sequentPermutation) {
    return isIdentity(sequentPermutation['hyp']) && isIdentity(sequentPermutation['cons']);
}

function isIdentity(permutation) {
    for (let i = 0; i < permutation.length; i++) {
        if (i !== permutation[i]) {
            return false;
        }
    }

    return true;
}

function invertPermutation(permutation) {
    let invertedPerm = [];
    for (let i of identity(permutation.length)) {
        invertedPerm.push(permutation.indexOf(i));
    }
    return invertedPerm;
}

// **********************
// CHECK PROOF COMPLETION
// **********************

function markAsComplete($container) {
    let $mainDiv = $container.children('div.proof');
    $mainDiv.addClass('complete');
}

function markAsIncomplete($container) {
    let $mainDiv = $container.children('div.proof');
    $mainDiv.removeClass('complete');
}

function isBinary($sequentTable) {
    return $sequentTable.hasClass('binary-rule');
}

function isProved($sequentTable) {
    return $sequentTable.data('proved') === true;
}

function markParentSequentsAsProved($sequentTable) {
    $sequentTable.data('proved', true);
    undoMarkAsNotProvable($sequentTable);
    undoMarkAsNotAutoProvable($sequentTable);

    let $parentSequentTable = getParentSequentTable($sequentTable);
    if ($parentSequentTable !== null) {
        if (isBinary($parentSequentTable)) {
            let $premises = getPremisesSequentTable($parentSequentTable);
            // We check all premises are proved
            if ($premises === null || !$premises.every(isProved)) {
                return;
            }
        }
        markParentSequentsAsProved($parentSequentTable);
    } else {
        let $container = $sequentTable.closest('.proof-container');
        markAsComplete($container);
    }
}

function getParentSequentTable($sequentTable) {
    if (!$sequentTable.is(':last-child')) {
        return $sequentTable.next();
    }

    let $div = $sequentTable.closest('div');
    if ($div.hasClass('proof')) {
        return null;
    }

    return $div.parent().next();
}

function getPremisesSequentTable($sequentTable) {
    let ruleRequest = $sequentTable.data('ruleRequest') || null;
    if (ruleRequest === null) {
        return [];
    }

    let $prev = $sequentTable.prev();

    if ($prev.prop('tagName') === 'TABLE') {
        return [$prev];
    }

    let $premises = [];
    $prev.children('div.sibling').each(function (i, sibling) {
        let $siblingTable = $(sibling).children('table').last();
        $premises = $premises.concat($siblingTable);
    })

    if ($premises.length < 2) {
        // Proof has not been completely set up
        return null;
    }

    return $premises;
}

// *************
// EXPORT AS COQ
// *************

function createExportBar($container) {
    let $exportBar = $('<div>', {'class': 'export-bar'})
        .append($('<span>').text('Export:'));

    let coqButton = createExportButton(
        'images/coq.png',
        'Export as Coq',
        function () { exportAsCoq($container); });
    $exportBar.append(coqButton);

    let latexButton = createExportButton(
        'images/camera.png',
        'Proof drawing',
        function () { openExportDialog($container); });
    $exportBar.append(latexButton);

    let shareButton = createExportButton(
        'images/share-icon.png',
        'Share proof URL',
        function () { shareProof($container); });
    $exportBar.append(shareButton);

    $container.append($exportBar);
}

function createExportButton(logoPath, title, onClick) {
    return $(`<img src="${logoPath}" title="${title}"  alt=""/>`)
        .addClass('export')
        .on('click', onClick);
}

function exportAsCoq($container) {
    // We get proof stored in HTML
    let proof = getProofAsJson($container);
    let notations = getNotations($container);

    $.ajax({
        type: 'POST',
        url: '/export_as_coq',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ proof, notations })),
        success: function(data)
        {
            let a = document.createElement('a');
            let binaryData = [];
            binaryData.push(data);
            let url = window.URL.createObjectURL(new Blob(binaryData, {type: "application/text"}))
            a.href = url;
            a.download = 'ccLLproof.v';
            document.body.append(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        },
        error: displayErrorIfNotImplemented($container)
    });
}


// ***************
// EXPORT AS LATEX
// ***************

function openExportDialog($container) {
    let exportDialog = $('#export-dialog');
    if (!exportDialog.find('.' + 'download-button').length) {
        createOption(exportDialog, false, 'Draw explicit exchange rules', function () {}, 'explicit-exchange-dialog');
        exportDialog.append($('<p>')
            .append($('<button type="button">')
                .addClass('download-button')
                .text('Download export')));
    }
    exportDialog.find('.' + 'download-button').off('click')
        .on('click', function () { onDownloadClick(exportDialog, $container); });
    exportDialog.dialog('open');
}

function onDownloadClick(exportDialog, $container) {
    let format = $('input[name=format]:checked', '#export-dialog').val();
    let implicitExchange = !$('#export-dialog').find('input' + '[type=checkbox]').is(':checked');
    exportAsLatex($container, format, implicitExchange);
    exportDialog.dialog('close');
}

function exportAsLatex($container, format, implicitExchange) {
    // We get proof stored in HTML
    let proof = getProofAsJson($container);
    let notations = getNotations($container);

    let httpRequest = new XMLHttpRequest();
    httpRequest.open('POST', `/export_as_latex?format=${format}&implicitExchange=${implicitExchange}`, true);
    httpRequest.responseType = 'blob';
    httpRequest.setRequestHeader('Content-Type', "application/json; charset=UTF-8");

    let extension = `.${format}`;
    if (format === 'ascii' || format === 'utf8') {
        extension = `_${format}.txt`;
    }

    httpRequest.onload = function (event) {
        if (httpRequest.status !== 200) {
            onError(httpRequest, event)
        } else {
            let blob = httpRequest.response;
            let a = document.createElement('a');
            let url = window.URL.createObjectURL(blob);
            a.href = url;
            a.download = `ccLLproof${extension}`;
            document.body.append(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        }
    };

    httpRequest.onerror= function(event) {
        onError(httpRequest, event)
    };

    httpRequest.send(compressJson(JSON.stringify({ proof, notations })));
}

// ***********
// SHARE PROOF
// ***********

function shareProof($container) {
    // We get proof stored in HTML
    let proof = getProofAsJson($container);
    let notations = getNotations($container);

    $.ajax({
        type: 'POST',
        url: '/compress_proof',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ proof, notations })),
        success: function(data)
        {
            addQueryParamInUrl('p', data, "Add compressed_proof in URL");
            copyUrlToClipboard();
            displayPedagogicInfo('Sharable URL has been copied to clipboard.', $container);
        },
        error: onAjaxError
    });
}

function clearSavedProof() {
    addQueryParamInUrl('p', null, 'Clear saved proof');
}

// *****************
// CHECK PROVABILITY
// *****************

function checkProvability($sequentTable) {
    if ($sequentTable.data('notProvable') === true || $sequentTable.data('notProvable') === false) {
        return;
    }

    $sequentTable.data('notProvable', false);
    let sequent = $sequentTable.data('sequentWithoutPermutation');
    let $container = $sequentTable.closest('.proof-container');
    let notations = getNotations($container);

    $.ajax({
        type: 'POST',
        url: '/is_sequent_provable',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ sequent, notations })),
        success: function(data)
        {
            if (data['is_provable'] === false) {
                recMarkAsNotProvable($sequentTable);
            }
        },
        error: onAjaxError
    });
}

function recheckSequentsProvability($container, onlyStatus) {
    let $mainSequentTable = $container.find('table').last();
    recRecheckSequentsProvability($mainSequentTable, onlyStatus);
}

function recRecheckSequentsProvability($sequentTable, onlyStatus) {
    let ruleRequest = $sequentTable.data('ruleRequest') || null;
    if (ruleRequest !== null) {
        let $prev = $sequentTable.prev();

        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                recRecheckSequentsProvability($prev, onlyStatus);
            } else {
                $prev.children('div.sibling').each(function (i, sibling) {
                    let $siblingTable = $(sibling).children('table').last();
                    recRecheckSequentsProvability($siblingTable, onlyStatus);
                })
            }
        }
    }

    if ((onlyStatus === 'notProvable' && $sequentTable.data('notProvable') === true)
        || (onlyStatus === 'provable' && $sequentTable.data('notProvable') === false)
        || (onlyStatus !== 'notProvable' && onlyStatus !== 'provable')) {
        undoMarkAsNotProvable($sequentTable);
        $sequentTable.data('notProvable', null);
        checkProvability($sequentTable);
    }
}

function recMarkAsNotProvable($sequentTable) {
    markAsNotProvable($sequentTable);

    let $parentSequentTable = getParentSequentTable($sequentTable);
    if ($parentSequentTable !== null) {
        let ruleRequest = $parentSequentTable.data('ruleRequest');
        if (isReversible(ruleRequest)) {
            recMarkAsNotProvable($parentSequentTable);
        }
    }
}

function isReversible(ruleRequest) {
    switch (ruleRequest.rule) {
        case "par":
        case "with":
        case "bottom":
        case "promotion":
            return true;

        default:
            return false;
    }
}

function markAsNotProvable($sequentTable) {
    $sequentTable.data('notProvable', true);
    let $turnstile = $sequentTable.find('span.turnstile');
    $turnstile.addClass('not-provable');
    $turnstile.attr('title', 'This sequent is not provable');
}

function undoMarkAsNotProvable($sequentTable) {
    $sequentTable.data('notProvable', false);
    let $turnstile = $sequentTable.find('span.turnstile');
    $turnstile.removeClass('not-provable');
    $turnstile.removeAttr('title');
}


// *******************
// AUTO-REVERSE OPTION
// *******************

function autoReverseContainer($container) {
    let $mainSequentTable = $container.find('table').last();
    autoReverseSequentPremises($mainSequentTable);
}

function autoReverseSequentPremises($sequentTable) {
    let $premisesSequentTables = getLastPremisesSequentTable($sequentTable);
    for (let $premiseSequentTable of $premisesSequentTables) {
        if (!isProved($premiseSequentTable)) {
            autoReverseSequent($premiseSequentTable);
        }
    }
}

function getLastPremisesSequentTable($sequentTable) {
    let ruleRequest = $sequentTable.data('ruleRequest') || null;
    if (ruleRequest !== null) {
        let $prev = $sequentTable.prev();

        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                return getLastPremisesSequentTable($prev);
            }

            let $premises = [];
            $prev.children('div.sibling').each(function (i, sibling) {
                let $siblingTable = $(sibling).children('table').last();
                let $siblingPremises = getLastPremisesSequentTable($siblingTable);
                $premises = $premises.concat($siblingPremises);
            })

            return $premises;
        }
    }

    return [$sequentTable];
}

function autoReverseSequent($sequentTable) {
    let $container = $sequentTable.closest('.proof-container');
    let options = $container.data('options');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentTable.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentTable);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    $.ajax({
        type: 'POST',
        url: '/auto_reverse_sequent',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(sequent)),
        success: function(data)
        {
            if (data.appliedRule !== null) {
                addPremises($sequentTable, permutationBeforeRule, data.appliedRule.ruleRequest, data.appliedRule.premises, options);
            }
        },
        error: onAjaxError
    });
}

// ********
// CUT MODE
// ********

function toggleCutMode($container, cutMode) {
    let $mainDiv = $container.children('div.proof');
    if (cutMode) {
        $mainDiv.addClass('cut-mode');
    } else {
        $mainDiv.removeClass('cut-mode');
    }
}

function addCutOnClick($commaOrPointSpan, isFirst) {
    $commaOrPointSpan.on('click', function () {
        let $sequentTable = $commaOrPointSpan.closest('table');
        let $container = $sequentTable.closest('.proof-container');
        let options = $container.data('options');

        if (options.cutMode.value) {
            openCutPopup(function (formula) {
                let formulaPosition = 0;
                if (!isFirst) {
                    let $li = $commaOrPointSpan.closest('li');
                    formulaPosition = $li.parent().children().index($li) + 1;
                }
                let ruleRequest = { rule: 'cut', formula, formulaPosition };
                applyRule(ruleRequest, $sequentTable);
            })
        }
    });
}

function openCutPopup(onFormulaSuccessCallback) {
    let $cutFormulaDialog = $('#cut-formula-dialog');
    let $textInput = $cutFormulaDialog.find($('input[name=formulaAsString]'));
    $textInput.select();
    $cutFormulaDialog.find('input' + '[type=submit]').off('click')
        .on('click', function () {
            let formulaAsString = $textInput.val();
            parseFormulaAsString(formulaAsString, function (formula) {
                $cutFormulaDialog.dialog('close');
                onFormulaSuccessCallback(formula);
            }, $cutFormulaDialog);
        })
    $cutFormulaDialog.dialog('open');
}

function parseFormulaAsString(formulaAsString, onFormulaSuccessCallback, $container) {
    $.ajax({
        type: 'GET',
        url: '/parse_formula',
        data: { formulaAsString },
        success: function(data)
        {
            if (data['is_valid']) {
                onFormulaSuccessCallback(data['formula']);
            } else {
                displayPedagogicError(data['error_message'], $container);
            }
        },
        error: onAjaxError
    });
}


// *********
// NOTATIONS
// *********

function createNotationBar($container, formulasAsString) {
    let $notationContainer = $('<div>');
    let $notationBar = $('<div>', {'class': 'notation-bar'});
    $notationContainer.append($notationBar
        .append($('<span>', {'class': 'notation-label'}).text('Add notation'))
        .append($('<span>', {'class': 'notation-add'})
            .text('+')
            .on('click', function () {
                let $form = createNotationForm(null, null);
                $form.insertBefore($notationBar);
            })));
    $container.append($notationContainer);

    for (let [notationName, notationValue] of formulasAsString) {
        let $form = createNotationForm(notationName, notationValue);
        $form.insertBefore($notationBar);
        $form.submit();
    }
}
function createNotationForm(defaultName, defaultFormulaAsString) {
    let editMode = !!defaultName;

    let $form = $('<form>')
        .addClass('notation-new-form')
        .append($('<input type="text" name="notationName" size="1">')
            .addClass('notation-new-input-name')
            .val(defaultName))
        .append($('<span>', {'class': 'notation-new-label'}).text('::='))
        .append($('<input type="text" name="notationFormulaAsString">')
            .val(defaultFormulaAsString));
    $form.append($('<span>', {'class': 'notation-new-button'})
        .text('✓')
        .on('click', function () { submitNotation($form, editMode); }));
    $form.append($('<span>', {'class': 'notation-new-button'})
        .text('⨯')
        .on('click', function () { removeNotationForm($form, editMode); }));
    $form.append('<input type="submit" style="visibility: hidden;position: absolute;" />');
    $form.on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
        submitNotation($form, editMode);
    });

    if (editMode) {
        $form.addClass('notation-item');
    }

    return $form;
}

function isValidNotationName(notationName, callbackIfValid, callbackIfNotValid) {
    $.ajax({
        type: 'GET',
        url: '/is_valid_litt',
        data: { litt: notationName },
        success: function(data)
        {
            if (data['is_valid']) {
                callbackIfValid();
            } else {
                callbackIfNotValid();
            }
        },
        error: onAjaxError
    });
}

function submitNotation($form, editMode) {
    let notationName = $form.find('input[name=notationName]').val();

    if (!editMode) {
        $form.addClass('notation-item');
    }
    let position = $form.parent().children('.notation-item').index($form);

    // For new notation name, we check that name not already exists
    if (!editMode || notationName !== getNotationNameByPosition($form, position)) {
        isValidNotationName(notationName, function () {
            if (getNotationByName($form, notationName) !== null) {
                displayPedagogicError(`Notation ${notationName} already exists.`, $form);
            } else {
                processFormulaAsString($form, notationName, position, editMode);
            }
        }, function () {
            displayPedagogicError(`Notation ${notationName} is not a valid litteral, please read the syntax rules.`, $form);
        });
    } else {
        processFormulaAsString($form, notationName, position, editMode);
    }
}

function processFormulaAsString($form, notationName, position, editMode) {
    let notationFormulaAsString = $form.find('input[name=notationFormulaAsString]').val();

    // For notation edition, we don't parse notationFormulaAsString if it hasn't change
    if (editMode) {
        let cachedFormula = getCachedFormula($form, position, notationFormulaAsString);
        if (cachedFormula !== null) {
            addNotation($form, notationName, notationFormulaAsString, cachedFormula, position, editMode);

            return;
        }
    }

    parseFormulaAsString(notationFormulaAsString, function(formula) {
        addNotation($form, notationName, notationFormulaAsString, formula, position, editMode);
    }, $form);
}

function addNotation($form, notationName, notationFormulaAsString, notationFormula, position, editMode) {
    // Save notation
    if (!editMode) {
        insertNewNotation($form, position, notationName, notationFormulaAsString, notationFormula);
    } else {
        setNotationByPosition($form, position, notationName, notationFormulaAsString, notationFormula);
    }

    // Display line
    let $notationLine = createNotationLine(notationName, notationFormulaAsString, notationFormula);
    $form.replaceWith($notationLine);
}

function createNotationLine(notationName, notationFormulaAsString, notationFormula) {
    let $notationLine = $('<div>', {'class': 'notation-line'})
        .addClass('notation-item')
        .data('notationName', notationName)
        .data('notationFormulaAsString', notationFormulaAsString)
        .data('notationFormula', notationFormula)
        .append($('<span>').text(notationName))
        .append($('<span>').text(' ::= '))
        .append($('<span>').html(createFormulaHTML(notationFormula, true)));
    $notationLine.on('click', function() { editNotationLine($notationLine); })

    return $notationLine;
}

function removeNotationForm($form, editMode) {
    if (editMode) {
        let position = $form.parent().children('.notation-item').index($form);
        removeNotationByPosition($form, position);
    }

    $form.remove();
}

function editNotationLine($notationLine) {
    let $form = createNotationForm($notationLine.data('notationName'), $notationLine.data('notationFormulaAsString'));
    $notationLine.replaceWith($form);
}

function getNotationByName($e, name) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');

    if (!options.notations) {
        return null;
    }

    for (let [notationName, notationFormulaAsString] of options.notations.formulasAsString) {
        if (name === notationName) {
            return notationFormulaAsString;
        }
    }

    return null;
}

function getNotationNameByPosition($e, position) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');

    return options.notations.formulasAsString[position][0];
}

function getCachedFormula($e, position, formulaAsString) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');
    if (formulaAsString === options.notations.formulasAsString[position][1]
        && options.notations.formulas.length > position) {
        return options.notations.formulas[position][1];
    }

    return null;
}

function insertNewNotation($e, position, name, formulaAsString, formula) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');
    if (position === options.notations.formulasAsString.length) {
        options.notations.formulasAsString.push([name, formulaAsString]);
        options.notations.formulas.push([name, formula]);
        options.notations.onAdd([name, formulaAsString]);
    } else {
        options.notations.formulasAsString.splice(position, 0, [name, formulaAsString]);
        options.notations.formulas.splice(position, 0, [name, formula]);
        options.notations.onUpdate(options.notations.formulasAsString);
    }

    $container.data('options', options);
    recheckSequentsProvability($container, 'notProbable');
}

function setNotationByPosition($e, position, name, formulaAsString, formula) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');

    if (name !== options.notations.formulasAsString[position][0]
        || formulaAsString !== options.notations.formulasAsString[position][1]) {
        options.notations.formulasAsString[position] = [name, formulaAsString];
        $container.data('options', options);
        options.notations.onUpdate(options.notations.formulasAsString);
    }

    if (position >= options.notations.formulas.length
        || name !== options.notations.formulas[position][0]
        || formula !== options.notations.formulas[position][1]) {
        options.notations.formulas[position] = [name, formula];
        $container.data('options', options);
        recheckSequentsProvability($container, null);
    }
}

function removeNotationByPosition($e, position) {
    let $container = $e.closest('.proof-container');
    let options = $container.data('options');
    options.notations.formulasAsString.splice(position,1);
    options.notations.formulas.splice(position,1);
    options.notations.onUpdate(options.notations.formulasAsString);
    $container.data('options', options);
    recheckSequentsProvability($container, 'provable');
}

function getNotations($container) {
    let options = $container.data('options');

    if (!options.notations?.formulas) {
        return [];
    }

    return options.notations.formulas;
}

// *****
// UTILS
// *****

function onAjaxError(jqXHR, textStatus, errorThrown) {
    console.log(jqXHR);
    console.log(jqXHR.responseText);
    console.log(textStatus);
    console.log(errorThrown);

    let alertText = 'Technical error, check browser console for more details.';
    if (jqXHR.responseText === 'Body content is too big') {
        alertText = 'Sorry, your proof exceeds the limit.';
    }
    alert(alertText);
}

function displayErrorIfNotImplemented($container) {
    return function (jqXHR, textStatus, errorThrown) {
        if (jqXHR.status === 501) {
            displayPedagogicError(jqXHR.responseText, $container);
        }
        else {
            onAjaxError(jqXHR, textStatus, errorThrown);
        }
    }
}

function onError(httpRequest, event) {
    console.log(event);
    console.log(httpRequest);
    console.log(httpRequest.statusText);

    alert('Technical error, check browser console for more details.');
}

function compressJson(json) {
    for (let [field, abbreviation] of Object.entries(ABBREVIATIONS)) {
        json = json.replaceAll(field, abbreviation);
    }
    return json;
}

function uncompressJson(json) {
    for (let [field, abbreviation] of Object.entries(ABBREVIATIONS)) {
        json = json.replaceAll(abbreviation, field);
    }
    return json;
}
