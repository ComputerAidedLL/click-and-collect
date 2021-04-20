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

function initProof(proofAsJson, $container, options) {
    let $div = $('<div>', {'class': 'proof'});
    createSubProof(proofAsJson, $div, options);
    $container.append($div);

    if (options.exportButtons) {
        createExportBar($container);
    }

    if (options.autoReverseOption) {
        createAutoReverseOption($container);
    }
}

function initProofWithSequent(sequent, $container, options) {
    initProof({sequent, appliedRule: null}, $container, options);
}

function createSubProof(proofAsJson, $subProofDivContainer, options) {
    let $sequentTable = createSequentTable(proofAsJson.sequent, options);
    $subProofDivContainer.prepend($sequentTable);
    let $sequentDiv = $sequentTable.find('div' + '.sequent');
    if (proofAsJson.appliedRule) {
        addPremises($sequentDiv,
            null,
            proofAsJson.appliedRule.ruleRequest,
            proofAsJson.appliedRule.premises,
            options);
    } else {
        if (options.checkProvability) {
            checkProvability($sequentDiv);
        }
    }
}

function createSequentTable(sequent, options) {
    let $table = $('<table>');

    let $td = $('<td>');
    $td.append(createSequent(sequent, options));
    $table.append($td);

    let $tagBox = $('<td>', {'class': 'tagBox'})
        .html('&nbsp;');
    $table.append($tagBox);

    return $table;
}

// **********
// APPLY RULE
// **********

function applyRule(ruleRequest, $sequentDiv, options) {
    let $container = $sequentDiv.closest('.proof-container');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentDiv);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    let autoReverseMode = false;
    if (options.autoReverseOption) {
        autoReverseMode = $container.children('.auto-reverse-bar').find('input').is(":checked");
    }

    $.ajax({
        type: 'POST',
        url: `/apply_rule?auto_reverse_mode=${autoReverseMode}`,
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify({ ruleRequest, sequent })),
        success: function(data)
        {
            if (data.success === true) {
                cleanPedagogicError($container);
                addPremises($sequentDiv, permutationBeforeRule, ruleRequest, data['premises'], options);
                markAsCompleteIfProofIsComplete($container);
            } else {
                displayPedagogicError(data['errorMessage'], $container);
            }
        },
        error: onAjaxError
    });
}

function addPremises($sequentDiv, permutationBeforeRule, ruleRequest, premises, options) {
    // Undo previously applied rule if any
    undoRule($sequentDiv);

    // Save data
    $sequentDiv
        .data('permutationBeforeRule', permutationBeforeRule)
        .data('ruleRequest', ruleRequest);

    // Add line
    let $td = $sequentDiv.closest('td');
    $td.addClass('inference');

    // Add rule symbol
    let $ruleSymbol = $('<div>', {'class': `tag ${ruleRequest.rule}`}).html(RULES[ruleRequest.rule]);
    if (options.withInteraction) {
        $ruleSymbol.addClass('clickable');
        $ruleSymbol.on('click', function() { undoRule($sequentDiv); })
    }
    $td.next('.tagBox').html($ruleSymbol);

    // Add premises
    let $table = $td.closest('table');
    if (premises.length === 1) {
        createSubProof(premises[0], $table.parent(), options);
    } else if (premises.length > 1) {
        let $div = $('<div>');
        $div.insertBefore($table);
        for (let premise of premises) {
            let $sibling = $('<div>', {'class': 'sibling'})
            $div.append($sibling);
            createSubProof(premise, $sibling, options)
        }
        $table.addClass('binary-rule');
    }
}

function undoRule($sequentDiv) {
    // Erase data
    $sequentDiv
        .data('permutationBeforeRule', null)
        .data('ruleRequest', null);

    // Remove line
    let $td = $sequentDiv.closest('td');
    $td.removeClass('inference');

    // Remove rule symbol
    $td.next('.tagBox').html('');

    // Remove premises
    let $table = $td.closest('table');
    $table.prevAll().each(function (i, e) {
        e.remove();
    });
    $table.removeClass('binary-rule');

    // Mark proof as incomplete
    let $container = $table.closest('.proof-container');
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
        $div.append($close)
            .insertAfter($container.children('div.proof'));
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

function recGetProofAsJson($table) {
    let $sequentDiv = $table.find('div.sequent');
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let ruleRequest = $sequentDiv.data('ruleRequest') || null;
    let appliedRule = null;
    if (ruleRequest !== null) {
        let $prev = $table.prev();
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

        let permutationBeforeRule = $sequentDiv.data('permutationBeforeRule');
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
    checkProofIsCompleteByAPI(proofAsJson, function() {
        markAsComplete($container);
    });
}

function checkProofIsCompleteByAPI(proofAsJson, callbackIfComplete) {
    $.ajax({
        type: 'POST',
        url: '/is_proof_complete',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(proofAsJson)),
        success: function(data)
        {
            if (data['is_complete'] === true) {
                callbackIfComplete();
            }
        },
        error: onAjaxError
    });
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

function checkProvability($sequentDiv) {
    let sequent = $sequentDiv.data('sequentWithoutPermutation');

    $.ajax({
        type: 'POST',
        url: '/is_sequent_provable',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(sequent)),
        success: function(data)
        {
            if (data['is_provable'] === false) {
                markAsNotProvable($sequentDiv);
            }
        },
        error: onAjaxError
    });
}

function markAsNotProvable($sequentDiv) {
    $sequentDiv.addClass('not-provable');
    $sequentDiv.find('span.turnstile').attr('title', 'This sequent is not provable');
}

// *******************
// AUTO-REVERSE OPTION
// *******************
function createAutoReverseOption($container) {
    let $autoReverseBar = $('<div>', {'class': 'auto-reverse-bar'})
        .append($('<span>', {'class': 'auto-reverse-label'})
            .text('Automatically apply reversible rules'))
        .append($('<label>', {'class': 'switch'})
            .html('<input type="checkbox"><span class="slider"></span>'));
    $container.append($autoReverseBar);
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
