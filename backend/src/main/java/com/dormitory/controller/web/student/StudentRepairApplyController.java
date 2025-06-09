package com.dormitory.controller.web.student;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.RepairApplyDTO;
import com.dormitory.controller.qry.RepairApplyQry;
import com.dormitory.controller.vo.RepairApplyVO;
import com.dormitory.service.RepairApplyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
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
@RequestMapping("/student/repair/apply")
@Api(value = "StudentRepairApplyController", tags = {"维修申请接口"})
public class StudentRepairApplyController {

    /**
     * 维修申请Service
     */
    private final RepairApplyService repairApplyService;

    @ApiOperation("学生维修申请分页查询")
    @GetMapping("/page")
    public R<IPage<RepairApplyVO>> studentPageByQry(RepairApplyQry qry) {
        return R.success(repairApplyService.studentPageByQry(qry));
    }

    @ApiOperation("维修申请详情")
    @GetMapping("/detail/{repairId}")
    public R<RepairApplyVO> detail(@PathVariable @NotNull(message = "申请ID") Long repairId) {
        return R.success(repairApplyService.detailById(repairId));
    }

    @ApiOperation("发起维修申请")
    @PostMapping("/initiate")
    public R<Boolean> initiate(@Valid @RequestBody RepairApplyDTO dto) {
        return R.success(repairApplyService.initiate(dto));
    }

    @ApiOperation("取消维修申请")
    @PostMapping("/cancel/{repairId}")
    public R<Boolean> cancel(@NotNull(message = "申请ID") @PathVariable Long repairId) {
        return R.success(repairApplyService.cancel(repairId));
    }
}

