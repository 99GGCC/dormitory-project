package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.ChangeApplyDTO;
import com.dormitory.controller.qry.ChangeApplyQry;
import com.dormitory.controller.vo.ChangeApplyVO;
import com.dormitory.service.ChangeApplyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 调换申请表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/change/apply")
@Api(value = "AdminChangeApplyController", tags = {"调换申请接口"})
public class AdminChangeApplyController {

    /**
     * 调换申请Service
     */
    private final ChangeApplyService changeApplyService;

    @ApiOperation("调换申请分页查询")
    @GetMapping("/page")
    public R<IPage<ChangeApplyVO>> pageByQry(ChangeApplyQry qry) {
        return R.success(changeApplyService.pageByQry(qry));
    }

    @ApiOperation("调换申请详情")
    @GetMapping("/detail/{changeId}")
    public R<ChangeApplyVO> detail(@PathVariable @NotNull(message = "申请ID") Long changeId) {
        return R.success(changeApplyService.detailById(changeId));
    }

    @ApiOperation("发起调换申请")
    @PostMapping("/initiate")
    public R<Boolean> initiate(@RequestBody @Valid ChangeApplyDTO dto) {
        return R.success(changeApplyService.initiate(dto));
    }

    @ApiOperation("处理调换申请")
    @PostMapping("/status/{changeId}")
    public R<Boolean> status(@NotNull(message = "申请ID") @PathVariable Long changeId,
                             @RequestParam Integer status,
                             @RequestParam String applyResult) {
        return R.success(changeApplyService.status(changeId, status, applyResult));
    }
}

