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
    'weakening': '?<span class="italic">w</span>'
};

// *************
// PROOF DISPLAY
// *************

function initProof(proofAsJson, $container, withInteraction) {
    let $div = $('<div>', {'class': 'proofIsIncomplete'});
    let $div2 = $('<div>', {'class': 'proof'});
    createSubProof(proofAsJson, $div2, withInteraction);
    $div.append($div2);
    $container.append($div);
}

function initProofWithSequent(sequentAsJson, $container, withInteraction) {
    initProof({sequentAsJson, appliedRule: null}, $container, withInteraction);
}

function createSubProof(proofAsJson, $subProofDivContainer, withInteraction) {
    let $sequentTable = createSequentTable(proofAsJson.sequentAsJson, withInteraction);
    $subProofDivContainer.prepend($sequentTable);
    let $sequentDiv = $sequentTable.find('div' + '.sequent');
    if (proofAsJson.appliedRule) {
        addPremises($sequentDiv,
            null,
            proofAsJson.appliedRule.rule,
            proofAsJson.appliedRule.formulaPositions,
            proofAsJson.appliedRule.premises,
            withInteraction);
    }
}

function createSequentTable(sequentAsJson, withInteraction) {
    let $table = $('<table>');

    let $td = $('<td>');
    $td.append(createSequent(sequentAsJson, withInteraction));
    $table.append($td);

    let $tagBox = $('<td>', {'class': 'tagBox'})
        .html('&nbsp;');
    $table.append($tagBox);

    return $table;
}

// **********
// APPLY RULE
// **********

function applyRule(rule, $sequentDiv, formulaPositions) {
    let $container = $sequentDiv.closest('.proof-container');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentDiv);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify({ ruleRequest: {rule, formulaPositions}, sequent}),
        success: function(data)
        {
            console.log(data);
            if (data.success === true) {
                cleanPedagogicError($container);
                let premises = data['sequentList'].map(function (sequentAsJson) {
                    return { sequentAsJson, appliedRule: null };
                });
                addPremises($sequentDiv, permutationBeforeRule, rule, formulaPositions, premises, true);
            } else {
                displayPedagogicError(data['errorMessage'], $container);
            }
        },
        error: onAjaxError
    });
}

function addPremises($sequentDiv, permutationBeforeRule, rule, formulaPositions, premises, withInteraction) {
    // Save data
    $sequentDiv
        .data('permutationBeforeRule', permutationBeforeRule)
        .data('rule', rule)
        .data('formulaPositions', formulaPositions);

    // Undo previously applied rule if any
    let $td = $sequentDiv.closest('td');
    undoRule($td);

    // Add line
    $td.addClass('inference');

    // Add rule symbol
    let $ruleSymbol = $('<div>', {'class': `tag ${rule}`}).html(RULES[rule]);
    if (withInteraction) {
        $ruleSymbol.addClass('clickable');
        $ruleSymbol.on('click', function() { undoRule($td); })
    }
    $td.next('.tagBox').html($ruleSymbol);

    // Add premises
    let $table = $td.closest('table');
    let $container = $table.closest('.proof-container');
    if (premises.length === 0) {
        if (withInteraction) {
            markAsCompleteIfProofIsComplete($container);
        }
    } else if (premises.length === 1) {
        createSubProof(premises[0], $table.parent(), withInteraction);
    } else {
        let $div = $('<div>');
        for (let premise of premises) {
            let $sibling = $('<div>', {'class': 'sibling'})
            createSubProof(premise, $sibling, withInteraction)
            $div.append($sibling);
        }
        $table.addClass('binary-rule');
        $div.insertBefore($table);
    }
}

function undoRule($td) {
    // Remove line
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
        $div.append($close);
        $container.append($div);
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
        .children('div')
        .children('div.proof')
        .children('table')
        .last();

    return recGetProofAsJson($mainTable);
}

function recGetProofAsJson($table) {
    let $sequentDiv = $table.find('div.sequent');
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let rule = $sequentDiv.data('rule') || null;
    let appliedRule = null;
    if (rule !== null) {
        let formulaPositions = $sequentDiv.data('formulaPositions');
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
        appliedRule = { ruleRequest: {rule, formulaPositions}, premises };

        let permutationBeforeRule = $sequentDiv.data('permutationBeforeRule');
        if (!isIdentitySequentPermutation(permutationBeforeRule)) {
            let sequentWithPermutation = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);
            appliedRule = {
                ruleRequest: {
                    rule: 'exchange',
                    formulaPositions: permutationBeforeRule['cons']
                },
                premises: [{sequentAsJson: sequentWithPermutation, appliedRule}]
            }
        }
    }

    return { sequentAsJson: sequentWithoutPermutation, appliedRule };
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
    let $mainDiv = $container.children('div');
    $mainDiv.removeClass('proofIsIncomplete');
    $mainDiv.addClass('proofIsDone');
}

function markAsIncomplete($container) {
    let $mainDiv = $container.children('div');
    $mainDiv.removeClass('proofIsDone');
    $mainDiv.addClass('proofIsIncomplete');
}

function markAsCompleteIfProofIsComplete($container) {
    // We get proof stored in HTML
    let proofAsJson = getProofAsJson($container);

    // We check if proof is complete
    checkProofIsComplete(proofAsJson, function() {
        markAsComplete($container);
        // createExportAsCoqButton($container);
    });
}

function checkProofIsComplete(proofAsJson, callbackIfComplete) {
    checkProofIsCompleteByAPI(proofAsJson, callbackIfComplete);

    // if (recCheckIsComplete(proofAsJson)) {
    //     callbackIfComplete();
    // }
}

function recCheckIsComplete(proofAsJson) {
    if (proofAsJson.appliedRule === null) {
        return false;
    }

    let response = true;

    for (let premiss of proofAsJson.appliedRule.premises) {
        response = response && recCheckIsComplete(premiss);
    }

    return response;
}

function checkProofIsCompleteByAPI(proofAsJson, callbackIfComplete) {
    $.ajax({
        type: 'POST',
        url: '/is_proof_complete',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify(proofAsJson),
        success: function(data)
        {
            console.log(data);
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

function createExportAsCoqButton($container) {
    let coqButton = $('<input type="button" value="Export as Coq"/>')
        .on('click', function () { exportAsCoq($container); });
    $container.append(coqButton);
}

function exportAsCoq($container) {
    // We get proof stored in HTML
    let proofAsJson = getProofAsJson($container);

    $.ajax({
        type: 'POST',
        url: '/export_as_coq',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify(proofAsJson),
        success: function(data)
        {
            console.log(data);

            let a = document.createElement('a');
            let binaryData = [];
            binaryData.push(data);
            let url = window.URL.createObjectURL(new Blob(binaryData, {type: "application/text"}))
            a.href = url;
            a.download = 'proof_as_coq.v';
            document.body.append(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        },
        error: onAjaxError
    });
}

function onAjaxError(jqXHR, textStatus, errorThrown) {
    console.log(jqXHR);
    console.log(jqXHR.responseText);
    console.log(textStatus);
    console.log(errorThrown);
    alert('Technical error, check browser console for more details.');
}
