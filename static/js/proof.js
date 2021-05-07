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
    'exchange': '<span class="italic">ech</span>'
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

    let $div = $('<div>', {'class': 'proof'});
    $container.append($div);
    createSubProof(proofAsJson, $div, options);

    if (options.exportButtons) {
        createExportBar($container);
    }

    if (options.autoReverse) {
        createOption($container, 'autoReverse', 'Auto-reverse',function (autoReverse) {
            if (autoReverse) {
                autoReverseContainer($container);
            }
            options.autoReverse.onToggle(autoReverse);
        }, options.autoReverse.dialog);

        if (options.autoReverse.value) {
            autoReverseContainer($container);
        }
    }
}

function createSubProof(proofAsJson, $subProofDivContainer, options) {
    let $sequentTable = createSequentTable(proofAsJson.sequent, options);
    $subProofDivContainer.prepend($sequentTable);
    if (proofAsJson.appliedRule) {
        let permutationBeforeRule = null;

        if (proofAsJson.appliedRule.ruleRequest.rule === 'exchange') {
            permutationBeforeRule = {'hyp': [], 'cons': proofAsJson.appliedRule.ruleRequest.permutation};
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

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ ruleRequest, sequent })),
        success: function(data)
        {
            if (data.success === true) {
                cleanPedagogicError($container);
                let appliedRule = data['proof'].appliedRule;
                addPremises($sequentTable, permutationBeforeRule, appliedRule.ruleRequest, appliedRule.premises, options);
                markAsCompleteIfProofIsComplete($container);

                if (!isSequentComplete($sequentTable) && options.autoReverse.value) {
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
    $td.addClass('inference');

    // Add rule symbol
    let $ruleSymbol = $('<div>', {'class': `tag ${ruleRequest.rule}`}).html(RULES[ruleRequest.rule]);
    if (options.withInteraction) {
        $ruleSymbol.addClass('clickable');
        $ruleSymbol.on('click', function() { undoRule($sequentTable); })
    }
    $td.next('.tagBox').html($ruleSymbol);

    // Add premises
    if (premises.length === 0) {
        markParentSequentsAsProved($sequentTable);
    } else if (premises.length === 1) {
        createSubProof(premises[0], $sequentTable.parent(), options);
    } else {
        let $div = $('<div>');
        $div.insertBefore($sequentTable);
        for (let premise of premises) {
            let $sibling = $('<div>', {'class': 'sibling'})
            $div.append($sibling);
            createSubProof(premise, $sibling, options)
        }
        $sequentTable.addClass('binary-rule');
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
    $td.removeClass('inference');

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

function displayPedagogicError(errorMessage, $container) {
    let $div = $container
        .children('div.pedagogic-error');
    if (!$div.length) {
        $div = $('<div>', {'class': 'pedagogic-error'});
        $div.append($('<div>', {'class': 'message'}));
        let $close = $('<div>', {'class': 'close-button'});
        $close.html('✖');
        $close.on('click', function () {cleanPedagogicError($container);});
        $div.append($close);

        let $proofDiv = $container.children('div.proof');
        if ($proofDiv.length) {
            $div.insertAfter($proofDiv);
        } else {
            $container.append($div);
        }
    }
    $div.children('div.message').text(errorMessage);
}

function cleanPedagogicError($container) {
    $container
        .children('div.pedagogic-error')
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
        if (!isIdentitySequentPermutation(permutationBeforeRule)) {
            let sequentWithPermutation = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);
            appliedRule = {
                ruleRequest: {
                    rule: 'exchange',
                    permutation: permutationBeforeRule['cons']
                },
                premises: [{sequent: sequentWithPermutation, appliedRule}]
            }
        }
    }

    return { sequent: sequentWithoutPermutation, appliedRule };
}

function isIdentitySequentPermutation(sequentPermutation) {
    if (sequentPermutation === null) {
        return true;
    }

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

function markAsCompleteIfProofIsComplete($container) {
    // We get proof stored in HTML
    let proofAsJson = getProofAsJson($container);

    // We check if proof is complete
    if (checkProofIsComplete(proofAsJson)) {
        markAsComplete($container);

        return true;
    }

    return false;
}

function isSequentComplete($sequentTable) {
    let proofAsJson = recGetProofAsJson($sequentTable);

    return checkProofIsComplete(proofAsJson);
}

function checkProofIsComplete(proofAsJson) {
    if (proofAsJson.appliedRule === null) {
        return false;
    }

    return proofAsJson.appliedRule.premises.every(checkProofIsComplete);
}

function isBinary($sequentTable) {
    return $sequentTable.hasClass('binary-rule');
}

function isProved($sequentTable) {
    return $sequentTable.data('proved') === true;
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

function markParentSequentsAsProved($sequentTable) {
    $sequentTable.data('proved', true);
    undoMarkAsNotProvable($sequentTable);
    undoMarkAsNotAutoProvable($sequentTable);

    let $parentSequentTable = getParentSequentTable($sequentTable);
    if ($parentSequentTable !== null) {
        if (!isBinary($parentSequentTable) || getPremisesSequentTable($parentSequentTable).every(isProved)) {
            markParentSequentsAsProved($parentSequentTable);
        }
    }
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
        'coq',
        function () { exportAsCoq($container); });
    $exportBar.append(coqButton);

    let latexButton = createExportButton(
        'images/LaTeX_logo.png',
        'Export as LaTeX',
        'latex',
        function () { exportAsLatex($container, 'tex'); });
    $exportBar.append(latexButton);

    let pdfButton = createExportButton(
        'images/pdf-icon.png',
        'Export as PDF',
        'pdf',
        function () { exportAsLatex($container, 'pdf'); });
    $exportBar.append(pdfButton);

    let pngButton = createExportButton(
        'images/camera.png',
        'Export as PNG',
        'png',
        function () { exportAsLatex($container, 'png'); });
    $exportBar.append(pngButton);

    $container.append($exportBar);
}

function createExportButton(logoPath, title, className, onClick) {
    return $(`<img src="${logoPath}" title="${title}"  alt=""/>`)
        .addClass(className)
        .addClass('export')
        .on('click', onClick);
}

function exportAsCoq($container) {
    // We get proof stored in HTML
    let proofAsJson = getProofAsJson($container);

    $.ajax({
        type: 'POST',
        url: '/export_as_coq',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(proofAsJson)),
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
        error: onAjaxError
    });
}


// ***************
// EXPORT AS LATEX
// ***************

function exportAsLatex($container, format) {
    // We get proof stored in HTML
    let proofAsJson = getProofAsJson($container);

    let httpRequest = new XMLHttpRequest();
    httpRequest.open('POST', `/export_as_latex?format=${format}`, true);
    httpRequest.responseType = 'blob';
    httpRequest.setRequestHeader('Content-Type', "application/json; charset=UTF-8");

    httpRequest.onload = function (event) {
        if (httpRequest.status !== 200) {
            onError(httpRequest, event)
        } else {
            let blob = httpRequest.response;
            let a = document.createElement('a');
            let url = window.URL.createObjectURL(blob);
            a.href = url;
            a.download = `ccLLproof.${format}`;
            document.body.append(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        }
    };

    httpRequest.onerror= function(event) {
        onError(httpRequest, event)
    };

    httpRequest.send(compressJson(JSON.stringify(proofAsJson)));
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

    $.ajax({
        type: 'POST',
        url: '/is_sequent_provable',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(sequent)),
        success: function(data)
        {
            if (data['is_provable'] === false) {
                markAsNotProvable($sequentTable);

                let $parentSequentTable = getParentSequentTable($sequentTable);
                if ($parentSequentTable !== null) {
                    checkProvability($parentSequentTable);
                }
            }
        },
        error: onAjaxError
    });
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

function createOption($container, optionName, text, onToggle, dialog) {
    let $input = $('<input type="checkbox">');
    let options = $container.data('options');
    $input.prop('checked', options[optionName].value);
    $input.on('change', function() {
        let options = $container.data('options');
        options[optionName].value = this.checked;
        $container.data('options', options);
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

function autoReverseContainer($container) {
    let $mainSequentTable = $container.find('table').last();
    autoReverseSequentPremises($mainSequentTable);
}

function autoReverseSequentPremises($sequentTable) {
    let $premisesSequentTables = getLastPremisesSequentTable($sequentTable);
    for (let $premiseSequentTable of $premisesSequentTables) {
        if (!isSequentComplete($premiseSequentTable)) {
            autoReverseSequent($premiseSequentTable);
        }
    }
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

    return $premises;
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
                markAsCompleteIfProofIsComplete($container);
            }
        },
        error: onAjaxError
    });
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
