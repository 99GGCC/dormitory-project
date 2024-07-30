package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.RepairApplyQry;
import com.dormitory.controller.vo.RepairApplyVO;
import com.dormitory.service.RepairApplyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 维修申请表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/repair/apply")
@Api(value = "AdminRepairApplyController", tags = {"维修申请接口"})
public class AdminRepairApplyController {

    /**
     * 维修申请Service
     */
    private final RepairApplyService repairApplyService;

    @ApiOperation("维修申请分页查询")
    @GetMapping("/page")
    public R<IPage<RepairApplyVO>> pageByQry(RepairApplyQry qry) {
        return R.success(repairApplyService.pageByQry(qry));
    }

    @ApiOperation("维修申请详情")
    @GetMapping("/detail/{repairId}")
    public R<RepairApplyVO> detail(@PathVariable @NotNull(message = "申请ID") Long repairId) {
        return R.success(repairApplyService.detailById(repairId));
    }

    @ApiOperation("处理维修申请")
    @PostMapping("/status/{repairId}")
    public R<Boolean> status(@NotNull(message = "申请ID") @PathVariable Long repairId,
                             @RequestParam Integer status) {
        return R.success(repairApplyService.status(repairId, status));
    }

}

